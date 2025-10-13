-- CHORDER - a chord instrument
-- @abhayadana
--
-- Out: mx.samples and/or MIDI
-- Diatonic chords on white keys
-- Long-press K3 for chord HUD

engine.name = "MxSamples"

local musicutil = require "musicutil"
local mxsamples = include "mx.samples/lib/mx.samples"

-- ===== config =====
local BASE = _path.audio .. "mx.samples"
local DEFAULT_MIDI_IN_NAME  = "Launchkey Mini MK3 1"
local DEFAULT_MIDI_OUT_PORT = 1

-- ===== state =====
local mx

-- UI pages
local PAGE_OUTPUT, PAGE_CHORD, PAGE_HUD = 1, 2, 3
local page = PAGE_OUTPUT

-- Root + scale
local root_pc, root_oct, root_midi = 0, 1, 24
local scale_name = "Major"

-- Chord build
local chord_type = 1         -- 1=triad, 2=7th, 3=9th
local sus_mode   = 1         -- 1=normal, 2=sus2, 3=sus4
local inversion = 0
local spread_semitones = 0

-- Voicing:
-- 1=none, 2=drop-2, 3=drop-3, 4=drop-2&4, 5=drop-1, 6=open, 7=wide,
-- 8=quartal, 9=quintal, 10=nearest, 11=smooth
local voicing_mode = 1
local add_bass = false

-- For voice-leading modes
local last_voiced_notes = nil
local last_bass_note = nil

-- Velocity (chord voice)
local velocity_mode = 1 -- 1=fixed, 2=incoming
local fixed_velocity = 100

-- Humanize (chord voice)
local humanize_steps_max = 0
local humanize_vel_range = 0

-- Timing / global clock usage
local quantize = false
local strum_steps = 0

-- Strum type
local STRUM_OPTS = {
  -- existing
  "up","down","up/down","down/up","random",
  "center-out","outside-in","bass-bounce","treble-bounce",
  "random no-repeat first","random stable ends",
  -- new (up)
  "edge-kiss","ping-pair","arp chunk 2–3","guitar rake","harp gliss split",
  -- new (down / interleaved)
  "arp chunk 2–3 ↓","guitar rake ↓","harp gliss split (interleaved)"
}
local strum_type = 1

-- Strum state (internal)
local strum_state = {
  alt_flip = false,
  last_first = nil,
  last_last  = nil
}

-- Quantize divisions (beats; 1 beat = quarter)
local QUANT_DIV_OPTS = {"1/1","1/2","1/3","1/4","1/6","1/8","1/12","1/16","1/24","1/32"}
local QUANT_DIV_MAP  = {["1/1"]=1/1, ["1/2"]=1/2, ["1/3"]=1/3, ["1/4"]=1/4, ["1/6"]=1/6, ["1/8"]=1/8, ["1/12"]=1/12, ["1/16"]=1/16, ["1/24"]=1/24, ["1/32"]=1/32}
local tick_div = 1/4
local tick_div_str = "1/4"

-- Swing
local swing_mode = 1
local swing_percent = 0
local swing_phase = 0

-- Event queue
local evq = {}

-- Scale cache
local SCALE_NAMES = {}
do
  for i = 1, #musicutil.SCALES do
    SCALE_NAMES[i] = musicutil.SCALES[i].name
  end
  if #SCALE_NAMES > 0 then scale_name = SCALE_NAMES[1] end
end

-- Instruments (mx.samples)
local display_names, canonical_names, folder_names = {}, {}, {}
local voice_index = 1
local loaded_set = {}

-- UI: chord banner
local last_chord_name = nil
local last_name_time = 0
local last_name_timeout = 2.0 -- kept hardcoded by request

-- ===== Free Play additions (Chord-enabled, independent) =====
local free_enable = false
local free_voice_index = 1
local FREE_OUT_MODE_PARAM = "free_out_mode"
local FREE_OUT_OPTS = {"mx.samples","mx.samples + MIDI","MIDI"}

-- Free Play: mode/recipe
local free_mode = 1           -- 1=mono, 2=chord
local free_seventh = 1        -- 1=triad, 2=7th
local free_inversion = 0
local free_spread = 0
local free_voicing = 1        -- same scale as VOICE_OPTS
local free_add_bass = 1       -- 1=off, 2=on

-- Free Play: strum + timing shapes (simple & independent)
local free_strum_steps = 0    -- in "steps" of tick_div; 0 = simultaneous
local free_strum_type = 1     -- independent from chord voice
local free_timing_shape = 1   -- 1=straight,2=serpentine,3=accel,4=rit,5=rake-in,6=rake-out,7=skip-alt
local free_timing_amt = 50    -- 0..100 (percent)
local free_timing_skip_steps = 1

-- Free Play: velocity/gate
local free_velocity_mode = 2  -- 1=fixed, 2=incoming
local free_fixed_velocity = 100
local free_gate_mode = 1      -- 1=release, else 25/50/75/100%

-- Free Play: routing + per-note/chord tracking
local free_mout = nil
local FREE_MIDI_OUT_DEV_PARAM = "free_midi_out_dev"
local FREE_MIDI_OUT_CH_PARAM  = "free_midi_out_ch"
local free_midi_channel = 1
local FREE_MIDI_IN_CH_PARAM = "free_midi_in_ch" -- explicit 1..16; default 2 per request
local free_transpose_oct = 0 -- octave transpose

local free_active_map = {}    -- mono path: [incoming_note] = quantized/transposed note
local free_chord_active = {}  -- chord path: [incoming_note] = {out_notes...}

-- HUD for Free Play chord symbol
local last_free_chord_name = nil
local last_free_name_time = 0

-- ===== mx volume (global; affects only mx.samples) =====
local mx_vol_pct = 100
local function mx_scaled_vel(v)
  local scaled = math.floor((v or 100) * (mx_vol_pct / 100))
  return util.clamp(scaled, 1, 127)
end

-- ===== utils =====
local function trim(s) return (s and s:gsub("^%s*(.-)%s*$", "%1")) or "" end
local function lower(s) return string.lower(s or "") end
local function sanitize_name(s) s = trim(s or ""); s = s:gsub("[/\\]+", ""); return trim(s) end
local function exists(path) local f=io.open(path,"r"); if f then f:close(); return true end; return false end
local function now() return util.time() end
math.randomseed(os.time())

-- Note names
local NOTE_NAMES_SHARP = {"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"}
local function midi_to_name_oct(m)
  local pc = m % 12
  local oct = math.floor(m / 12) - 1
  return NOTE_NAMES_SHARP[pc+1], oct
end
local function recompute_root_midi()
  root_midi = (root_oct + 1) * 12 + root_pc
end

-- UI helpers
local function ellipsize(s, max_chars) s = tostring(s or ""); if #s <= max_chars then return s end; return s:sub(1, math.max(0, max_chars - 1)) .. "…" end
local function draw_header(active_label)
  screen.level(15); screen.move(4, 12); screen.text("CHORDER")
  screen.level(10); screen.move(124, 12); if active_label then screen.text_right("["..active_label.."]") end
end
local function draw_line(y, label, value)
  screen.level(12); screen.move(4, y); screen.text(label or "")
  screen.level(15); screen.move(124, y); screen.text_right(value or "")
end

-- compact labels for output modes
local function short_mode_name(mode)
  if     mode == "mx.samples"        then return "mx"
  elseif mode == "mx.samples + MIDI" then return "mx+M"
  elseif mode == "MIDI"              then return "MIDI"
  else return mode or "?" end
end

-- ===== mx.samples helpers =====
local function manifest_name_for_folder(folder)
  local fpath = BASE .. "/" .. folder .. "/manifest.lua"
  if not exists(fpath) then return sanitize_name(folder) end
  local ok, ret = pcall(dofile, fpath)
  if ok and type(ret) == "table" then
    if type(ret.name) == "string" and ret.name ~= "" then return sanitize_name(ret.name) end
    if ret.info and type(ret.info.name) == "string" then return sanitize_name(ret.info.name) end
    if ret.meta and type(ret.meta.name) == "string" then return sanitize_name(ret.meta.name) end
  end
  return sanitize_name(folder)
end

local function scan_instruments()
  local entries = util.scandir(BASE) or {}
  local tmp = {}
  for _, e in ipairs(entries) do
    if e:sub(1,1) ~= "." then
      local canon = manifest_name_for_folder(e)
      table.insert(tmp, {display=canon, canon=canon, folder=e})
    end
  end
  table.sort(tmp, function(a,b) return a.display:lower() < b.display:lower() end)
  display_names, canonical_names, folder_names = {}, {}, {}
  for i,rec in ipairs(tmp) do
    display_names[i]   = rec.display
    canonical_names[i] = rec.canon
    folder_names[i]    = rec.folder
  end
  print("Found "..tostring(#display_names).." mx.samples instrument(s).")
end

local function load_folder_into_helper(folder)
  if not folder or folder == "" then return false end
  if loaded_set[lower(folder)] then return true end
  local path = BASE .. "/" .. folder
  local ok = false
  if mx and mx.load_folder then
    ok = pcall(function() mx:load_folder(path) end)
  elseif mx and mx.add_folder then
    ok = pcall(function() mx:add_folder(path) end)
  end
  if ok then
    loaded_set[lower(folder)] = true
    print("mx.samples: loaded '"..folder.."'")
    return true
  else
    print("mx.samples: failed to load '"..folder.."' at "..path)
    return false
  end
end

local function ensure_selected_loaded()
  local folder = folder_names[voice_index]
  if folder then load_folder_into_helper(folder) end
end

-- central safe wrapper: includes per-hit amp/gain for audible mx volume
local function mx_on_safe(canon_name, midi_note, vel127)
  canon_name = sanitize_name(canon_name); if canon_name == "" then return end
  local ok, err = pcall(function()
    mx:on({
      name = canon_name,
      midi = midi_note,
      velocity = util.clamp(vel127,1,127),
      amp = mx_vol_pct / 100,   -- per-note amplitude
      gain = mx_vol_pct / 100,  -- alias if engine prefers 'gain'
    })
  end)
  if not ok then print("mx:on error: "..tostring(err).." (name='"..canon_name.."')") end
end
local function mx_off_safe(canon_name, midi_note)
  canon_name = sanitize_name(canon_name); if canon_name == "" then return end
  local ok, err = pcall(function() mx:off({ name = canon_name, midi = midi_note }) end)
  if not ok then print("mx:off error: "..tostring(err).." (name='"..canon_name.."')") end
end

-- ===== MIDI out (Awake-style; nbout friendly) =====
local midi_devices = {}
local midi_ports_map = {}
local mout = nil
local midi_channel = 1

local MIDI_OUT_DEV_PARAM = "chorder_midi_out_dev"
local MIDI_OUT_CH_PARAM  = "chorder_midi_out_ch"

local active_notes = {}
local function active_notes_clear() active_notes = {} end
local function active_note_on(nn) active_notes[nn] = true end
local function active_note_off(nn) active_notes[nn] = nil end

local function all_midi_notes_off()
  if not mout then active_notes_clear(); return end
  for n,_ in pairs(active_notes) do pcall(function() mout:note_off(n, 0, midi_channel) end) end
  active_notes_clear()
end

local function build_midi_device_list_awake()
  midi_devices = {}
  midi_ports_map = {}
  for i = 1, #midi.vports do
    local long_name = midi.vports[i].name or ("dev "..i)
    local short_name = (string.len(long_name) > 15) and util.acronym(long_name) or long_name
    table.insert(midi_devices, (i..": "..short_name))
    midi_ports_map[#midi_devices] = i
  end
  if #midi_devices == 0 then
    table.insert(midi_devices, "1: (no devices)")
    midi_ports_map[1] = 1
  end
end

local function setup_midi_out_awake(param_index)
  local real_port = midi_ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
  if mout then mout.event = nil; mout = nil end
  mout = midi.connect(real_port)
  if mout then
    print("MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
  else
    print("MIDI OUT: connect failed for port "..real_port)
  end
end

local function midi_out_on_awake(note, vel127)
  if not mout then return end
  local vel = util.clamp(vel127, 1, 127)
  local n   = util.clamp(note,   0, 127)
  local ok, err = pcall(function() mout:note_on(n, vel, midi_channel) end)
  if not ok then print("MIDI OUT note_on error: "..tostring(err)) end
  active_note_on(n)
end

-- Per-note MIDI gate timing (replaces single metro approach)
local MIDI_GATE_PARAM = "chorder_midi_gate"
local MIDI_GATE_OPTS = {"release","25%","50%","75%","100%"}
local function compute_gate_seconds()
  local sel = params:get(MIDI_GATE_PARAM) or 1
  if sel == 1 then return nil end
  local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
  local bpm = clock.get_tempo()
  return (60 / bpm) * (tick_div or 1/4) * frac
end
local function schedule_gate_for_note(note)
  local secs = compute_gate_seconds()
  if not secs or secs <= 0 then return end
  local n = util.clamp(note, 0, 127)
  clock.run(function()
    clock.sleep(secs)
    if active_notes[n] then
      pcall(function() mout:note_off(n, 0, midi_channel) end)
      active_note_off(n)
    end
  end)
end

-- ===== chord math =====
local WHITE_SET = { [0]=true,[2]=true,[4]=true,[5]=true,[7]=true,[9]=true,[11]=true }
local WHITE_TO_DEG = { [0]=1, [2]=2, [4]=3, [5]=4, [7]=5, [9]=6, [11]=7 }

local function scale128() return musicutil.generate_scale_of_length(root_midi, scale_name, 128) end
local function nearest_index_with_degree(sc, midi, target_deg)
  local best_i, best_d = 1, 999
  for i=1,#sc do
    if 1 + ((i-1) % 7) == target_deg then
      local d = math.abs(sc[i] - midi)
      if d < best_d then best_d, best_i = d, i end
    end
  end
  return best_i
end

local function triad_quality(notes)
  table.sort(notes)
  local a = notes[2] - notes[1]
  local b = notes[3] - notes[2]
  if a == 4 and b == 3 then return "maj" end
  if a == 3 and b == 4 then return "min" end
  if a == 3 and b == 3 then return "dim" end
  if a == 4 and b == 4 then return "aug" end
  return "unk"
end

-- Helpers for voicing
local function clone_sorted(t)
  local c = {}
  for i,n in ipairs(t) do c[i]=n end
  table.sort(c)
  return c
end

local function apply_drop(t, which_from_top)
  local v = clone_sorted(t)
  if #v < which_from_top then return v end
  local idx = #v - which_from_top + 1
  v[idx] = v[idx] - 12
  table.sort(v)
  return v
end

local function apply_drop_multiple(t, list_from_top)
  local v = clone_sorted(t)
  table.sort(list_from_top)
  for i=#list_from_top,1,-1 do
    v = apply_drop(v, list_from_top[i])
  end
  return v
end

local function apply_open_position(t)
  local v = clone_sorted(t)
  if #v == 3 then
    v = { v[1], v[3], v[2] + 12 }
  elseif #v == 4 then
    v = { v[1], v[3], v[4], v[2] + 12 }
  end
  table.sort(v)
  return v
end

local function apply_wide_spread(t)
  local v = clone_sorted(t)
  for i=2,#v,2 do v[i] = v[i] + 12 end
  table.sort(v)
  return v
end

local function diatonic_stack(sc, i_root, steps, count)
  local idxs = {}
  for k=0,(count-1) do
    idxs[#idxs+1] = i_root + k*steps
  end
  return idxs
end

local function assign_register_nearest(target, prev)
  local out = clone_sorted(target)
  if not prev or #prev == 0 or #prev ~= #out then
    local center = 60
    local base = out[1]
    local shift = math.floor((center - base) / 12)
    for i=1,#out do out[i] = out[i] + shift*12 end
    table.sort(out)
    return out
  end
  local v = {}
  for i=1,#out do v[i] = out[i] end
  local prev_low = prev[1]
  local base = v[1]
  local shift = math.floor((prev_low - base) / 12)
  for i=1,#v do v[i] = v[i] + shift*12 end
  for i=1,#v do
    local cur = v[i]
    local want = prev[i]
    local best = cur
    local bestd = math.abs(cur - want)
    local a = cur - 12; local ad = math.abs(a - want)
    if ad < bestd then best, bestd = a, ad end
    local b = cur + 12; local bd = math.abs(b - want)
    if bd < bestd then best, bestd = b, bd end
    v[i] = best
  end
  table.sort(v)
  return v
end

local function choose_smooth_inversion(base_idxs, sc, want7)
  if not last_bass_note then return base_idxs end
  local best_idxs = base_idxs
  local best = nil
  local max_inv = math.max(0, (want7 and 3 or 2))
  for inv=0,max_inv do
    local idxs = {}
    for _,ix in ipairs(base_idxs) do idxs[#idxs+1] = ix end
    for _=1, math.min(inv, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
    local n0 = sc[util.clamp(idxs[1], 1, #sc)]
    local d = math.abs(n0 - last_bass_note)
    if not best or d < best then best = d; best_idxs = idxs end
  end
  return best_idxs
end

local function apply_voicing(notes, base_idxs, sc, use7)
  local mode = voicing_mode or 1
  if mode == 1 then
    return notes
  elseif mode == 2 then
    return apply_drop(notes, 2)
  elseif mode == 3 then
    return apply_drop(notes, 3)
  elseif mode == 4 then
    return apply_drop_multiple(notes, {2,4})
  elseif mode == 5 then
    return apply_drop(notes, 1)
  elseif mode == 6 then
    return apply_open_position(notes)
  elseif mode == 7 then
    return apply_wide_spread(notes)
  elseif mode == 8 or mode == 9 then
    local steps = (mode == 8) and 3 or 4
    local count = use7 and 4 or 3
    local i_root = base_idxs[1]
    local idxs = diatonic_stack(sc, i_root, steps, count)
    local out = {}
    for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out)
    return out
  elseif mode == 10 then
    return assign_register_nearest(notes, last_voiced_notes)
  elseif mode == 11 then
    local idxs = choose_smooth_inversion(base_idxs, sc, use7)
    local out = {}
    for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out)
    return out
  end
  return notes
end

-- Build chord tones, return (notes ascending, quality, naming_root_midi)
local function chord_for_degree(base_midi, deg, chord_type_sel, inv, spread, sus_sel)
  local sc = scale128()
  local i_root = nearest_index_with_degree(sc, base_midi, deg)

  -- tertian base stack: 1-3-5 (optionally 7 and 9)
  local idxs = { i_root, i_root+2, i_root+4 }
  local want7 = (chord_type_sel >= 2)
  local want9 = (chord_type_sel >= 3)
  if want7 then idxs[#idxs+1] = i_root+6 end
  if want9 then idxs[#idxs+1] = i_root+8 end

  -- third handling (sus): replace the third with 2 or 4
  if sus_sel and sus_sel ~= 1 then
    for k = 1, #idxs do
      if idxs[k] == i_root+2 then -- the 3rd
        if sus_sel == 2 then       -- sus2
          idxs[k] = i_root+1
        elseif sus_sel == 3 then   -- sus4
          idxs[k] = i_root+3
        end
        break
      end
    end
  end

  -- apply explicit inversion unless smooth-voiceleading mode picks it
  local inversion_is_forced = (voicing_mode ~= 11)
  if inversion_is_forced then
    for _ = 1, math.min(inv or 0, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
  end

  -- build close voicing in base register (+ spread semitones)
  local notes = {}
  for _,ix in ipairs(idxs) do
    local n = sc[util.clamp(ix, 1, #sc)]
    notes[#notes+1] = n + (spread or 0)
  end

  -- quality + naming root (quality still computed from 1-3-5 if present)
  local triad3 = {notes[1], notes[2], notes[3]}
  local quality = triad_quality(triad3)
  local name_root_midi = sc[i_root]

  -- apply selected voicing (may re-register / rebuild)
  notes = apply_voicing(notes, idxs, sc, want7)

  -- optional bass (root -12)
  if add_bass then notes[#notes+1] = (notes[1] - 12) end

  table.sort(notes)
  return notes, quality, name_root_midi
end

-- naming helpers
local function base_chord_symbol(root_pc_val, qual, chord_type_sel, sus_sel)
  local root_txt = NOTE_NAMES_SHARP[(root_pc_val % 12) + 1]

  -- If suspended, we ignore major/minor triad quality in the symbol and use sus2/sus4.
  local sus_suffix = (sus_sel == 2 and "sus2") or (sus_sel == 3 and "sus4") or nil

  -- chord_type: 1=triad, 2=7th, 3=9th (9th implies 7th)
  if sus_suffix then
    if     chord_type_sel == 1 then return root_txt .. sus_suffix
    elseif chord_type_sel == 2 then return root_txt .. "7"  .. sus_suffix
    elseif chord_type_sel == 3 then return root_txt .. "9"  .. sus_suffix
    end
    return root_txt .. sus_suffix
  else
    if chord_type_sel == 3 then
      -- 9th chord names
      if qual == "maj" then return root_txt .. "maj9" end
      if qual == "min" then return root_txt .. "m9" end
      if qual == "dim" then return root_txt .. "m9b5" end -- simple convention
      if qual == "aug" then return root_txt .. "+9" end
      return root_txt .. "9"
    elseif chord_type_sel == 2 then
      -- 7th chord names
      if qual == "maj" then return root_txt .. "maj7" end
      if qual == "min" then return root_txt .. "m7" end
      if qual == "dim" then return root_txt .. "m7b5" end
      if qual == "aug" then return root_txt .. "+7" end
      return root_txt .. "7"
    else
      -- triads
      if qual == "maj" then return root_txt end
      if qual == "min" then return root_txt .. "m" end
      if qual == "dim" then return root_txt .. "dim" end
      if qual == "aug" then return root_txt .. "+" end
      return root_txt
    end
  end
end

local function build_chord_display_name(notes, qual, name_root_midi)
  if type(notes) ~= "table" or #notes == 0 then return "" end
  local root_pc_val = name_root_midi % 12
  local symbol = base_chord_symbol(root_pc_val, qual, chord_type, sus_mode)
  local bass = notes[1]; for i=2,#notes do if notes[i] < bass then bass = notes[i] end end
  local bass_pc = bass % 12
  if bass_pc ~= root_pc_val then symbol = symbol .. "/" .. NOTE_NAMES_SHARP[bass_pc + 1] end
  local tags = {}
  if inversion > 0 and (voicing_mode ~= 11) then table.insert(tags, "inv"..inversion) end
  if voicing_mode == 2 then table.insert(tags, "drop2")
  elseif voicing_mode == 3 then table.insert(tags, "drop3")
  elseif voicing_mode == 4 then table.insert(tags, "drop2&4")
  elseif voicing_mode == 5 then table.insert(tags, "drop1")
  elseif voicing_mode == 6 then table.insert(tags, "open")
  elseif voicing_mode == 7 then table.insert(tags, "wide")
  elseif voicing_mode == 8 then table.insert(tags, "quartal")
  elseif voicing_mode == 9 then table.insert(tags, "quintal")
  elseif voicing_mode == 10 then table.insert(tags, "nearest")
  elseif voicing_mode == 11 then table.insert(tags, "smooth") end
  if add_bass and bass_pc == root_pc_val then table.insert(tags, "bass") end
  if spread_semitones ~= 0 then
    local s = (spread_semitones > 0) and ("+"..spread_semitones) or tostring(spread_semitones)
    table.insert(tags, "spread"..s)
  end
  if #tags > 0 then symbol = symbol .. " (" .. table.concat(tags, ", ") .. ")" end
  return symbol
end

-- Free Play naming (independent recipe)
local function build_free_chord_display_name(notes, qual, name_root_midi)
  if type(notes) ~= "table" or #notes == 0 then return "" end
  local root_pc_val = name_root_midi % 12
  local free_chord_type = (free_seventh == 2) and 2 or 1
  local symbol = base_chord_symbol(root_pc_val, qual, free_chord_type, 1) -- sus normal
  local bass = notes[1]; for i=2,#notes do if notes[i] < bass then bass = notes[i] end end
  local bass_pc = bass % 12
  if bass_pc ~= root_pc_val then symbol = symbol .. "/" .. NOTE_NAMES_SHARP[bass_pc + 1] end
  local tags = {}
  if free_inversion > 0 and (free_voicing ~= 11) then table.insert(tags, "inv"..free_inversion) end
  if free_voicing == 2 then table.insert(tags, "drop2")
  elseif free_voicing == 3 then table.insert(tags, "drop3")
  elseif free_voicing == 4 then table.insert(tags, "drop2&4")
  elseif free_voicing == 5 then table.insert(tags, "drop1")
  elseif free_voicing == 6 then table.insert(tags, "open")
  elseif free_voicing == 7 then table.insert(tags, "wide")
  elseif free_voicing == 8 then table.insert(tags, "quartal")
  elseif free_voicing == 9 then table.insert(tags, "quintal")
  elseif free_voicing == 10 then table.insert(tags, "nearest")
  elseif free_voicing == 11 then table.insert(tags, "smooth") end
  if (free_add_bass == 2) and bass_pc == root_pc_val then table.insert(tags, "bass") end
  if free_spread ~= 0 then
    local s = (free_spread > 0) and ("+"..free_spread) or tostring(free_spread)
    table.insert(tags, "spread"..s)
  end
  if #tags > 0 then symbol = symbol .. " (" .. table.concat(tags, ", ") .. ")" end
  return symbol
end

-- ===== event queue / clock =====
local function queue_in_steps(steps, fn) table.insert(evq, {steps=math.max(0, steps), fn=fn}) end
local function schedule(step_steps, fn) if not quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

-- Free Play scheduling (always immediate; ignores global quantize)
local function free_schedule(step_steps, fn) if step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

local function clock_loop()
  while true do
    local len = tick_div
    if swing_mode == 2 and swing_percent > 0 then
      local s = swing_percent / 100
      if swing_phase == 0 then len = tick_div * (1 + s) else len = tick_div * (1 - s) end
      swing_phase = 1 - swing_phase
    else
      swing_phase = 0
    end

    clock.sync(len)

    local remain = {}
    for _,e in ipairs(evq) do
      e.steps = e.steps - 1
      if e.steps <= 0 then e.fn() else table.insert(remain, e) end
    end
    evq = remain

    redraw()
  end
end

-- ===== MIDI In =====
local m = nil
local midi_in_devices = {"none"}
local midi_in_ports_map = { [1] = 0 }
local MIDI_IN_DEV_PARAM = "chorder_midi_in_dev"
local MIDI_IN_CH_PARAM  = "chorder_midi_in_ch" -- 1=omni, else ch+1

local function build_midi_in_device_list_awake()
  midi_in_devices = {"none"}
  midi_in_ports_map = { [1] = 0 }
  for i = 1, #midi.vports do
    local long_name  = midi.vports[i].name or ("dev "..i)
    local short_name = (#long_name > 15) and util.acronym(long_name) or long_name
    table.insert(midi_in_devices, (i..": "..short_name))
    midi_in_ports_map[#midi_in_devices] = i
  end
end

local function teardown_midi_in()
  if m then m.event = nil; m = nil end
end

-- ======= IMPORTANT: forward declaration for Option A =======
local make_strum_order_pure
-- ===========================================================

-- ===== Free Play helpers =====
local function free_load_selected()
  local folder = folder_names[free_voice_index]
  if folder then load_folder_into_helper(folder) end
end

-- Collect & format active Free Play notes (mono path)
local function free_active_notes_ascending()
  local t = {}
  for _,q in pairs(free_active_map) do t[#t+1] = q end
  table.sort(t)
  return t
end
local function free_active_names()
  local t = free_active_notes_ascending()
  if #t == 0 then return nil end
  local parts = {}
  for _,n in ipairs(t) do
    local nm, oc = midi_to_name_oct(n)
    parts[#parts+1] = string.format("%s%d", nm, oc)
  end
  return table.concat(parts, " ")
end

-- Simple Free Play strum order (STATELESS per trigger)
local function free_make_strum_order(count)
  local tmp_state = { alt_flip=false, last_first=nil, last_last=nil }
  local ord = select(1, make_strum_order_pure(count, free_strum_type, tmp_state))
  return ord or {}
end

-- Free Play timing shapes (simple)
local function free_compute_step_offsets(m)
  local base = math.max(0, free_strum_steps or 0)
  local offs = {}
  if m <= 1 or base == 0 then
    for k=1,m do offs[k] = 0 end
    return offs
  end

  local amt = (free_timing_amt or 50) / 100.0
  local spacing = {}
  for k=1,m-1 do spacing[k] = base end

  local function apply_serpentine()
    local mid = (m+1)/2
    for k=1,m-1 do
      local d = math.abs((k - mid) / mid)
      spacing[k] = base * (1 + amt * d)
    end
  end
  local function apply_accel()
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      spacing[k] = math.max(0.1, base * (1 - amt * t))
    end
  end
  local function apply_rit()
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      spacing[k] = base * (1 + amt * t)
    end
  end
  local function ease_in_quad(t)  return t*t end
  local function ease_out_quad(t) return 1 - (1-t)*(1-t) end
  local function apply_rake(ease_fn)
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      local w = ease_fn(t)
      spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5)*2))
      spacing[k] = math.max(0.1, spacing[k])
    end
  end
  local function apply_skip_alt()
    local add = (free_timing_skip_steps or 1) * amt
    for k=1,m-1 do
      if (k % 2) == 1 then spacing[k] = spacing[k] + add end
    end
  end

  if     free_timing_shape == 2 then apply_serpentine()
  elseif free_timing_shape == 3 then apply_accel()
  elseif free_timing_shape == 4 then apply_rit()
  elseif free_timing_shape == 5 then apply_rake(ease_in_quad)
  elseif free_timing_shape == 6 then apply_rake(ease_out_quad)
  elseif free_timing_shape == 7 then apply_skip_alt()
  end

  local sum = 0
  for k=1,m do
    if k == 1 then offs[k] = 0
    else sum = sum + util.round(spacing[k-1]); offs[k] = sum end
  end
  return offs
end

-- Free Play MIDI gate computation
local function free_compute_gate_seconds()
  local sel = free_gate_mode or 1
  if sel == 1 then return nil end
  local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
  local bpm = clock.get_tempo()
  return (60 / bpm) * (tick_div or 1/4) * frac
end

local function free_schedule_gate_for_note(note)
  local secs = free_compute_gate_seconds()
  if not secs or secs <= 0 then return end
  local n = util.clamp(note, 0, 127)
  clock.run(function()
    clock.sleep(secs)
    -- Gate affects BOTH MIDI and mx:
    if free_mout then pcall(function() free_mout:note_off(n, 0, free_midi_channel) end) end
    if mx then
      local fcanon = canonical_names[free_voice_index] or ""
      mx_off_safe(fcanon, n)
    end
  end)
end

-- Free Play output selection
local function free_want_mx()
  local mval = params:get(FREE_OUT_MODE_PARAM) or 1
  return (mval==1) or (mval==2)
end
local function free_want_midi()
  local mval = params:get(FREE_OUT_MODE_PARAM) or 1
  return (mval==2) or (mval==3)
end

local function setup_free_midi_out_awake(param_index)
  local real_port = midi_ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
  if free_mout then free_mout.event = nil; free_mout = nil end
  free_mout = midi.connect(real_port)
  if free_mout then
    print("FREE MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
  else
    print("FREE MIDI OUT: connect failed for port "..real_port)
  end
end

local function free_note_on_mx(canon, note, vel) mx_on_safe(canon, note, mx_scaled_vel(vel)) end
local function free_note_off_mx(canon, note) mx_off_safe(canon, note) end
local function free_note_on_midi(note, vel)
  if free_mout then pcall(function() free_mout:note_on(util.clamp(note,0,127), util.clamp(vel or 100,1,127), free_midi_channel) end) end
end
local function free_note_off_midi(note)
  if free_mout then pcall(function() free_mout:note_off(util.clamp(note,0,127), 0, free_midi_channel) end) end
end
local function free_fanout_on(canon, note, vel)
  if free_want_mx() then free_note_on_mx(canon, note, vel) end
  if free_want_midi() then free_note_on_midi(note, vel) end
end
local function free_fanout_off(canon, note)
  if free_want_mx() then free_note_off_mx(canon, note) end
  if free_want_midi() then free_note_off_midi(note) end
end

-- Free Play note handlers (mono/chord)
local function free_chord_note_on(in_note, in_vel)
  local pc = in_note % 12
  local deg = WHITE_TO_DEG[pc]
  if not deg then return end

  local sc = scale128()
  local i_deg = nearest_index_with_degree(sc, in_note, deg)
  local base = sc[util.clamp(i_deg, 1, #sc)]
  base = util.clamp(base + 12 * (free_transpose_oct or 0), 0, 127)

  local fcanon = canonical_names[free_voice_index] or ""
  local play_vel = (free_velocity_mode==1) and free_fixed_velocity or util.clamp(in_vel or 100,1,127)

  if free_mode == 1 then
    -- MONO path
    free_active_map[in_note] = base
    free_fanout_on(fcanon, base, play_vel)
    return
  end

  -- CHORD path (independent recipe; stateless strum)
  local free_chord_type = (free_seventh == 2) and 2 or 1  -- 1=triad, 2=7th

  -- Temporarily override global voicing/add_bass for building
  local keep_voicing, keep_add = voicing_mode, add_bass
  voicing_mode, add_bass = free_voicing, (free_add_bass==2)
  local chord_notes, qual, name_root_midi = chord_for_degree(base, deg, free_chord_type, free_inversion, free_spread, 1) -- sus normal
  voicing_mode, add_bass = keep_voicing, keep_add

  table.sort(chord_notes)

  local ord = (#chord_notes>1 and free_strum_steps>0) and free_make_strum_order(#chord_notes) or (function(n) local t={} for i=1,n do t[i]=i end; return t end)(#chord_notes)
  local offs = free_compute_step_offsets(#ord)

  local emitted = {}
  for k,idx in ipairs(ord) do
    local n = chord_notes[idx]
    local s = offs[k] or 0
    free_schedule(s, function()
      free_fanout_on(fcanon, n, play_vel)
      -- Gate affects BOTH MIDI and mx:
      free_schedule_gate_for_note(n)
    end)
    emitted[#emitted+1] = n
  end

  free_chord_active[in_note] = emitted

  -- HUD (Free Play chord symbol)
  last_free_chord_name = build_free_chord_display_name(chord_notes, qual, name_root_midi)
  last_free_name_time = now()
end

local function free_chord_note_off(in_note)
  if free_mode == 1 then
    local q = free_active_map[in_note]
    if q ~= nil then
      free_active_map[in_note] = nil
      local fcanon = canonical_names[free_voice_index] or ""
      free_fanout_off(fcanon, q)
    end
    return
  end
  local outs = free_chord_active[in_note]
  if outs then
    local fcanon = canonical_names[free_voice_index] or ""
    for _,n in ipairs(outs) do free_fanout_off(fcanon, n) end
    free_chord_active[in_note] = nil
  end
end

-- ===== Panic helpers =====
local function all_free_notes_off()
  local fcanon = canonical_names[free_voice_index] or ""
  -- mono actives
  for _,q in pairs(free_active_map) do
    free_fanout_off(fcanon, q)
  end
  -- chord actives
  for _,outs in pairs(free_chord_active) do
    for _,n in ipairs(outs) do free_fanout_off(fcanon, n) end
  end
  free_active_map = {}
  free_chord_active = {}
end

local function panic_all_outputs()
  if mx and mx.all_notes_off then pcall(function() mx:all_notes_off() end) end
  all_midi_notes_off()
  all_free_notes_off()
end

-- Forward declarations (so MIDI handler can call them)
local handle_note_on, handle_note_off

-- Unified setup for MIDI IN
local function setup_midi_in(param_index)
  teardown_midi_in()
  local port = midi_in_ports_map[param_index] or 0
  if port == 0 then print("MIDI IN: no input selected"); return end

  m = midi.connect(port)
  if not m then
    print("MIDI IN: connect failed for port "..port)
    return
  end

  m.event = function(data)
    local msg = midi.to_msg(data); if not msg then return end

    -- chord input channel filter (supports omni)
    local chord_ok = false
    do
      local ch_sel_idx = params:get(MIDI_IN_CH_PARAM) -- 1=omni, else ch+1
      if ch_sel_idx == 1 then chord_ok = true
      else chord_ok = (msg.ch == (ch_sel_idx - 1)) end
    end

    -- free play input channel (explicit 1..16; default 2)
    local free_ok = false
    do
      local free_ch = params:get(FREE_MIDI_IN_CH_PARAM) or 2
      free_ok = (msg.ch == free_ch)
    end

    local canon = canonical_names[voice_index] or ""
    local chord_vel = (velocity_mode == 2 and (msg.vel or fixed_velocity) or fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      -- CHORD VOICE (white keys -> diatonic chord degrees)
      if chord_ok then
        ensure_selected_loaded()
        local pc = msg.note % 12
        if WHITE_SET[pc] then
          local deg = WHITE_TO_DEG[pc]
          if deg then handle_note_on(canon, msg.note, chord_vel, deg) end
        end
      end

      -- FREE PLAY (mono or chord; immediate)
      if free_enable and free_ok then
        local vel = (free_velocity_mode==1) and free_fixed_velocity or msg.vel
        free_chord_note_on(msg.note, vel)
      end

      redraw()

    elseif (msg.type == "note_off") or (msg.type == "note_on" and msg.vel == 0) then
      -- CHORD VOICE offs
      if chord_ok then
        local pc = msg.note % 12
        if WHITE_SET[pc] then handle_note_off(canon, msg.note) end
      end

      -- FREE PLAY offs
      if free_enable and free_ok then
        free_chord_note_off(msg.note)
      end

      redraw()
    end
  end

  print("MIDI IN: connected to "..(midi_in_devices[param_index] or ("port "..port)))
end

local function rebind_midi_in_if_needed()
  if (m == nil) or (m.event == nil) then
    local cur = params:get(MIDI_IN_DEV_PARAM) or 1
    setup_midi_in(cur)
  end
end

local function rebuild_midi_lists(keep_in, keep_out)
  local remembered_in_long = nil
  do
    local cur_idx = params:get(MIDI_IN_DEV_PARAM) or 1
    local vp = midi_in_ports_map[cur_idx]
    if vp and vp > 0 and midi.vports[vp] then
      remembered_in_long = midi.vports[vp].name
    end
  end

  build_midi_device_list_awake()   -- outputs
  build_midi_in_device_list_awake()-- inputs

  -- refresh IN param options
  params:hide(MIDI_IN_DEV_PARAM);  params:show(MIDI_IN_DEV_PARAM)
  local p_in = params:lookup_param(MIDI_IN_DEV_PARAM)
  if p_in then
    p_in.options = midi_in_devices
    p_in.count = #midi_in_devices
  end

  -- refresh primary MIDI OUT device options
  params:hide(MIDI_OUT_DEV_PARAM); params:show(MIDI_OUT_DEV_PARAM)
  local p_out = params:lookup_param(MIDI_OUT_DEV_PARAM)
  if p_out then
    p_out.options = midi_devices
    p_out.count   = #midi_devices
  end

  -- refresh FREE PLAY MIDI OUT device options
  params:hide(FREE_MIDI_OUT_DEV_PARAM); params:show(FREE_MIDI_OUT_DEV_PARAM)
  local p_free_out = params:lookup_param(FREE_MIDI_OUT_DEV_PARAM)
  if p_free_out then
    p_free_out.options = midi_devices
    p_free_out.count   = #midi_devices
  end

  if keep_in and remembered_in_long then
    local idx = 1
    for i = 2, #midi_in_devices do
      local vp = midi_in_ports_map[i]
      if vp and midi.vports[vp] and midi.vports[vp].name == remembered_in_long then
        idx = i; break
      end
    end
    params:set(MIDI_IN_DEV_PARAM, idx)
    setup_midi_in(idx)
  end
end

-- ===== Strum order (Chord voice) =====
local function reverse_inplace(t)
  local i, j = 1, #t
  while i < j do
    t[i], t[j] = t[j], t[i]
    i = i + 1
    j = j - 1
  end
end

local function shuffle_inplace(t)
  for i = #t, 2, -1 do
    local j = math.random(1, i)
    t[i], t[j] = t[j], t[i]
  end
end

-- (Option A) assign to previously-declared local
make_strum_order_pure = function(count, stype, state)
  state = state or { alt_flip=false, last_first=nil, last_last=nil }
  local order = {}
  for i = 1, math.max(count or 0, 0) do order[i] = i end
  if #order <= 1 then return order, state end

  local function center_out(n)
    local seq = {}
    local lo = math.floor((n+1)/2)
    local hi = lo + 1
    if n % 2 == 1 then
      table.insert(seq, lo)
      while (#seq < n) do
        if hi <= n then table.insert(seq, hi) end
        local left = lo - (#seq % 2 == 0 and 0 or 1)
        if left >= 1 and #seq < n then table.insert(seq, left) end
        hi = hi + 1
      end
    else
      local a, b = n/2, n/2 + 1
      table.insert(seq, a); table.insert(seq, b)
      local step = 1
      while #seq < n do
        local l = a - step
        local r = b + step
        if l >= 1 then table.insert(seq, l) end
        if r <= n and #seq < n then table.insert(seq, r) end
        step = step + 1
      end
    end
    return seq
  end

  local function outside_in(n)
    local seq = {}
    local i, j = 1, n
    while i <= j do
      table.insert(seq, i)
      if i ~= j then table.insert(seq, j) end
      i = i + 1; j = j - 1
    end
    return seq
  end

  -- NEW helpers
  local function edge_kiss(n)
    local seq = {}
    local i, j = 1, n
    while i <= j do
      table.insert(seq, i)
      if i ~= j then table.insert(seq, j) end
      i = i + 1; j = j - 1
    end
    return seq
  end

  local function ping_pair(n)
    local seq = {}
    local L, R = 1, n
    while L < R do
      table.insert(seq, L); if L+1 <= R then table.insert(seq, L+1) end
      if R-1 >= L+2 then table.insert(seq, R-1) end
      table.insert(seq, R)
      L = L + 2; R = R - 2
    end
    if L == R then table.insert(seq, L) end
    return seq
  end

  local function arp_chunk_2_3(n, direction_up)
    local base = {}
    if direction_up then for i=1,n do base[#base+1] = i end
    else for i=n,1,-1 do base[#base+1] = i end end
    local seq, i, toggle = {}, 1, true
    while i <= #base do
      local size = toggle and 2 or 3
      for k=i, math.min(i+size-1, #base) do seq[#seq+1] = base[k] end
      i = i + size
      toggle = not toggle
    end
    return seq
  end

  local function guitar_rake(n, up)
    local seq = {}
    if up then for i=1,n do seq[#seq+1]=i end else for i=n,1,-1 do seq[#seq+1]=i end end
    return seq
  end

  local function harp_gliss_split(n)
    local mid = math.floor(n/2)
    local lo = {}; for i=1,mid do lo[#lo+1] = i end
    local hi = {}; for i=mid+1,n do hi[#hi+1] = i end
    local seq = {}
    for i=1,#lo do seq[#seq+1] = lo[i] end
    for i=1,#hi do seq[#seq+1] = hi[i] end
    return seq
  end

  local function harp_gliss_split_interleaved(n)
    local mid = math.floor(n/2)
    local lo = {}; for i=1,mid do lo[#lo+1] = i end
    local hi = {}; for i=mid+1,n do hi[#hi+1] = i end
    local seq = {}
    local i = 1
    while i <= math.max(#lo, #hi) do
      if i <= #lo then seq[#seq+1] = lo[i] end
      if i <= #hi then seq[#seq+1] = hi[i] end
      i = i + 1
    end
    return seq
  end

  local n = #order
  if     stype == 1  then return order, state
  elseif stype == 2  then reverse_inplace(order); return order, state
  elseif stype == 3  then if state.alt_flip then reverse_inplace(order) end; state.alt_flip = not state.alt_flip; return order, state
  elseif stype == 4  then if not state.alt_flip then reverse_inplace(order) end; state.alt_flip = not state.alt_flip; return order, state
  elseif stype == 5  then shuffle_inplace(order); state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 6  then local seq=center_out(n); for k=1,n do order[k]=seq[k] end; state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 7  then local seq=outside_in(n); for k=1,n do order[k]=seq[k] end; state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 8  then local seq=outside_in(n); for k=1,n do order[k]=seq[k] end; state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 9  then local seq=outside_in(n); reverse_inplace(seq); for k=1,n do order[k]=seq[k] end; state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 10 then
    local tries=0; repeat shuffle_inplace(order); tries=tries+1 until (order[1]~=state.last_first) or tries>8
    state.last_first=order[1]; state.last_last=order[#order]; return order, state
  elseif stype == 11 then
    if state.last_first and state.last_last and n >= 3 then
      local middle = {}
      for i=1,n do if i ~= state.last_first and i ~= state.last_last then middle[#middle+1] = i end end
      shuffle_inplace(middle)
      local out = { state.last_first }
      for i=1,#middle do out[#out+1] = middle[i] end
      out[#out+1] = state.last_last
      return out, state
    end
    shuffle_inplace(order); state.last_first=order[1]; state.last_last=order[#order]; return order, state

  -- NEW (up)
  elseif stype == 12 then return edge_kiss(n), state
  elseif stype == 13 then return ping_pair(n), state
  elseif stype == 14 then return arp_chunk_2_3(n, true), state
  elseif stype == 15 then return guitar_rake(n, true), state
  elseif stype == 16 then return harp_gliss_split(n), state

  -- NEW (down / interleaved)
  elseif stype == 17 then return arp_chunk_2_3(n, false), state
  elseif stype == 18 then return guitar_rake(n, false), state
  elseif stype == 19 then return harp_gliss_split_interleaved(n), state
  end

  return order, state
end

local function make_strum_order(count)
  local ord, ns = make_strum_order_pure(count, strum_type, strum_state)
  strum_state = ns
  return ord
end

-- ===== Timing helpers (Chord voice) =====
local function compute_step_offsets(m)
  local base = math.max(0, strum_steps or 0)
  if m <= 1 or base == 0 then
    local offs = {}
    for k=1,m do offs[k-1] = 0 end
    return offs
  end

  local shape = params:get("chorder_timing_shape") or 1
  local amt   = (params:get("chorder_timing_amt") or 50) / 100.0

  -- start with uniform spacing per hop
  local spacing = {}
  for k=1,m-1 do spacing[k] = base end

  local function apply_serpentine()
    local mid = (m+1)/2
    for k=1,m-1 do
      local d = math.abs((k - mid) / mid)
      spacing[k] = base * (1 + amt * d)
    end
  end

  local function apply_accel()
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      spacing[k] = math.max(0.1, base * (1 - amt * t))
    end
  end

  local function apply_rit()
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      spacing[k] = base * (1 + amt * t)
    end
  end

  local function ease_in_quad(t)  return t*t end
  local function ease_out_quad(t) return 1 - (1-t)*(1-t) end

  local function apply_rake(ease_fn)
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2))
      local w = ease_fn(t)
      spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5)*2))
      spacing[k] = math.max(0.1, spacing[k])
    end
  end

  local function apply_skip_alt()
    local add = params:get("chorder_timing_skip_steps") or 1
    local add_eff = add * amt
    for k=1,m-1 do
      if (k % 2) == 1 then
        spacing[k] = spacing[k] + add_eff
      end
    end
  end

  if     shape == 2 then apply_serpentine()
  elseif shape == 3 then apply_accel()
  elseif shape == 4 then apply_rit()
  elseif shape == 5 then apply_rake(ease_in_quad)
  elseif shape == 6 then apply_rake(ease_out_quad)
  elseif shape == 7 then apply_skip_alt()
  end

  local offs = {}
  local sum = 0
  offs[0] = 0
  for k=1,m-1 do
    sum = sum + spacing[k]
    offs[k] = util.round(sum)
  end
  return offs
end

-- ===== Velocity post-shaping (Chord voice) =====
local function apply_velocity_profile(k, m, base_vel, note_idx_in_sorted, n_sorted)
  local v = base_vel

  local ramp = params:get("chorder_ramp_per_step") or 0
  v = v + (k-1) * ramp

  local acc_tgt = params:get("chorder_accent") or 1
  local acc_amt = params:get("chorder_accent_amt") or 0
  if acc_tgt == 2 then -- bass
    if note_idx_in_sorted == 1 then v = v + acc_amt end
  elseif acc_tgt == 3 then -- top
    if note_idx_in_sorted == n_sorted then v = v + acc_amt end
  elseif acc_tgt == 4 then -- middle
    local mid = math.ceil(n_sorted/2)
    if note_idx_in_sorted == mid then v = v + acc_amt end
  end

  return util.clamp(math.floor(v), 1, 127)
end

-- ===== Output mode =====
local OUT_MODE_PARAM = "chorder_out_mode"
local OUT_OPTS = {"mx.samples", "mx.samples + MIDI", "MIDI"}
local function want_mx()   local mval = params:get(OUT_MODE_PARAM) or 1; return (mval==1) or (mval==2) end
local function want_midi() local mval = params:get(OUT_MODE_PARAM) or 1; return (mval==2) or (mval==3) end

local function fanout_note_on(canon, note, vel)
  if want_mx()   then mx_on_safe(canon, note, mx_scaled_vel(vel)) end
  if want_midi() then
    midi_out_on_awake(note, vel)
    schedule_gate_for_note(note) -- per-note gate
  end
end

local function fanout_note_off(canon, note)
  if want_mx()   then mx_off_safe(canon, note) end
  if want_midi() then
    if active_notes[note] then
      pcall(function() mout:note_off(note, 0, midi_channel) end)
      active_note_off(note)
    end
  end
end

local function jitter_steps(max_steps) if max_steps <= 0 then return 0 end; return math.random(0, max_steps) end
local function jitter_velocity(vel, range) if range <= 0 then return vel end; local d = math.random(-range, range); return util.clamp(vel + d, 1, 127) end

-- (forward-declared above)
handle_note_on = function(canon, root_note, vel, deg)
  local chord, qual, name_root_midi = chord_for_degree(root_note, deg, chord_type, inversion, spread_semitones, sus_mode)
  last_voiced_notes = {}
  for i,n in ipairs(chord) do last_voiced_notes[i] = n end
  last_bass_note = chord[1]

  last_chord_name = build_chord_display_name(chord, qual, name_root_midi); last_name_time = now()

  -- play order & offsets
  local order = make_strum_order(#chord)
  local offs  = compute_step_offsets(#order)

  -- precompute sorted index map for accent logic
  local sorted = clone_sorted(chord)
  local idx_in_sorted = {} -- midi -> first index in sorted
  for i,n in ipairs(sorted) do if idx_in_sorted[n]==nil then idx_in_sorted[n]=i end end
  local n_sorted = #sorted

  -- flam settings
  local flam_on    = (params:get("chorder_flam") or 1) == 2
  local flam_cnt   = params:get("chorder_flam_count") or 0
  local flam_space = params:get("chorder_flam_space") or 1
  local flam_dvel  = params:get("chorder_flam_vel") or -8

  for k,idx in ipairs(order) do
    local n = chord[idx]

    local s = offs[k-1] or 0
    s = s + jitter_steps(humanize_steps_max)

    local v_base = jitter_velocity(vel, humanize_vel_range)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = apply_velocity_profile(k, #order, v_base, pos_in_sorted, n_sorted)

    schedule(s, function() fanout_note_on(canon, n, v) end)

    if flam_on and flam_cnt > 0 then
      for j=1, flam_cnt do
        local ds = j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        schedule(s + ds, function() fanout_note_on(canon, n, vv) end)
      end
    end
  end
end

handle_note_off = function(canon, root_note)
  local pc = root_note % 12
  local deg = WHITE_TO_DEG[pc]
  if not deg then return end
  local chord = chord_for_degree(root_note, deg, chord_type, inversion, spread_semitones, sus_mode)
  local notes = (type(chord)=="table" and chord[1] and type(chord[1])=="number") and chord or {}
  for _,n in ipairs(notes) do fanout_note_off(canon, n) end
end

-- ===== Minimal UI =====
local function key_center_string()
  local name = NOTE_NAMES_SHARP[root_pc+1]
  local rname, roct = midi_to_name_oct(root_midi)
  return string.format("%s (root %s%d)", name, rname, roct)
end

-- Compact Output page (patched to show mx instrument and no Gate line)
local function draw_output_page()
  draw_header("I/O")

  -- chord output summary
  local out_mode_full  = OUT_OPTS[params:get(OUT_MODE_PARAM) or 1] or "?"
  local out_mode_short = short_mode_name(out_mode_full)

  local out_idx  = params:get(MIDI_OUT_DEV_PARAM) or 1
  local out_lbl  = ellipsize(midi_devices[out_idx] or "—", 12)
  local mo_ch    = tostring(params:get(MIDI_OUT_CH_PARAM) or 1)

  -- chord input summary
  local mi_i      = params:get(MIDI_IN_DEV_PARAM) or 1
  local mi_lbl    = ellipsize(midi_in_devices[mi_i] or "none", 12)
  local ch_in_idx = params:get(MIDI_IN_CH_PARAM) or 1
  local ch_in_disp = (ch_in_idx==1) and "Omni" or ("Ch"..tostring(ch_in_idx-1))

  -- free play summary
  local free_on        = (params:get("free_enable")==2)
  local free_mode_full = FREE_OUT_OPTS[params:get(FREE_OUT_MODE_PARAM) or 1] or "?"
  local free_mode_short = short_mode_name(free_mode_full)
  local free_dev_i     = params:get(FREE_MIDI_OUT_DEV_PARAM) or 1
  local free_dev       = ellipsize(midi_devices[free_dev_i] or "—", 12)
  local free_ch        = tostring(params:get(FREE_MIDI_OUT_CH_PARAM) or 1)
  local free_in_ch     = tostring(params:get(FREE_MIDI_IN_CH_PARAM) or 2)

  -- Row 1: Chord Out
  screen.level(12); screen.move(4, 22); screen.text("Out:")
  screen.level(15); screen.move(124,22)
  if out_mode_short == "MIDI" or out_mode_short == "mx+M" then
    screen.text_right(out_mode_short.." | "..out_lbl.." / Ch"..mo_ch)
  else
    screen.text_right(out_mode_short)
  end

  -- Row 2: Chord In
  screen.level(12); screen.move(4, 34); screen.text("In:")
  screen.level(15); screen.move(124,34); screen.text_right(mi_lbl.." / "..ch_in_disp)

  -- Row 3: Free
  screen.level(12); screen.move(4, 46); screen.text("Free:")
  screen.level(15); screen.move(124,46)
  if free_on then
    screen.text_right("on | "..free_mode_short.." | InCh "..free_in_ch)
  else
    screen.text_right("off")
  end

  -- Row 4: mx.samples instrument (only if output includes mx.samples)
  if want_mx() then
    local cur_name = display_names[voice_index] or "(no packs)"
    screen.level(12); screen.move(4, 58); screen.text("mx.samples:")
    screen.level(15); screen.move(124,58); screen.text_right(ellipsize(cur_name, 18))
  -- else if Free is on and includes MIDI, show its device; otherwise just BPM (no Gate)
  elseif free_on and (free_mode_short == "MIDI" or free_mode_short == "mx+M") then
    screen.level(12); screen.move(4, 58); screen.text("Free MIDI:")
    screen.level(15); screen.move(124,58); screen.text_right(free_dev.." / Ch"..free_ch)
  else
    local bpm = string.format("%d BPM", math.floor(clock.get_tempo() or 120))
    screen.level(10); screen.move(4, 58); screen.text(bpm)
  end
end

local function draw_chord_page()
  draw_header("Chord")
  draw_line(28, "Key:", key_center_string())
  draw_line(40, "Scale:", scale_name)

  -- Free Play currently-held notes (compact)
  local fp = free_active_names()
  screen.level(fp and 12 or 10)
  screen.move(64, 50)
  screen.text_center("Free: " .. (fp or "—"))

  local now_t = now()
  local show_name = last_chord_name and ((now_t - last_name_time) < last_name_timeout)
  screen.level(15)
  if show_name then
    screen.move(64, 60); screen.text_center(ellipsize(last_chord_name, 26))
  else
    screen.level(10); screen.move(64, 60); screen.text_center("(play a chord)")
  end
end

local function draw_hud_page()
  screen.clear()
  local now_t = now()
  local show_chord = last_chord_name and ((now_t - last_name_time) < last_name_timeout)
  local show_free  = last_free_chord_name and ((now_t - last_free_name_time) < last_name_timeout)

  -- Main chord banner
  if show_chord then
    screen.level(15)
    if screen.font_size then pcall(function() screen.font_size(12) end) end
    screen.move(64, 28); screen.text_center(ellipsize(last_chord_name, 26))
    if screen.font_size then pcall(function() screen.font_size(8) end) end
  else
    screen.level(10); screen.move(64, 28); screen.text_center("(play a chord)")
  end

  -- Free Play chord symbol (independent)
  if show_free then
    screen.level(12)
    if screen.font_size then pcall(function() screen.font_size(10) end) end
    screen.move(64, 44); screen.text_center(ellipsize(last_free_chord_name, 26))
    if screen.font_size then pcall(function() screen.font_size(8) end) end
  else
    screen.level(10); screen.move(64, 44); screen.text_center("(free chord)")
  end

  -- Free Play currently-held notes
  local fp = free_active_names()
  screen.level(fp and 12 or 10)
  screen.move(64, 58)
  screen.text_center("Free: " .. (fp or "—"))

  screen.update()
end

function redraw()
  screen.clear()
  if page == PAGE_HUD then draw_hud_page(); return end
  if page == PAGE_OUTPUT then draw_output_page() else draw_chord_page() end
  screen.update()
end

-- ===== lifecycle =====

-- helper to hide/show
local function show_param(id, show)
  if show then params:show(id) else params:hide(id) end
end

-- -------- PARAM RE-ORG HELPERS (unique separator IDs) --------
local _sep_counter = 0
local function add_section(title, builders)
  params:add_group(title, #builders)
  for _,fn in ipairs(builders) do fn() end
end
local function div(label)
  return function()
    _sep_counter = _sep_counter + 1
    params:add_separator(("— %s — [%d]"):format(label or "", _sep_counter))
  end
end
-- -------------------------------------------------------------

function init()
  mx = mxsamples:new()
  scan_instruments()
  build_midi_device_list_awake()
  build_midi_in_device_list_awake()

  -- ========= SECTION 1: CHORDER · I/O =========
  local default_midi_in_index = (function(long_name)
    if not long_name or long_name == "" then return 1 end
    for i = 2, #midi_in_devices do
      local vp = midi_in_ports_map[i]
      local ln = midi.vports[vp] and midi.vports[vp].name
      if ln == long_name then return i end
    end
    return 1
  end)(DEFAULT_MIDI_IN_NAME)

  local g_io = {
    div("Output"),
    function()
      params:add_option(OUT_MODE_PARAM, "output", OUT_OPTS, 1)
      params:set_action(OUT_MODE_PARAM, function(_)
        local includes_midi = want_midi()
        show_param(MIDI_GATE_PARAM, includes_midi) -- hide chord MIDI gate unless output includes MIDI
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,

    div("MIDI Out"),
    function()
      params:add_option(MIDI_OUT_DEV_PARAM, "MIDI output", midi_devices, 1)
      params:set_action(MIDI_OUT_DEV_PARAM, function(i)
        panic_all_outputs()
        setup_midi_out_awake(i)
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,
    function()
      params:add_option(MIDI_OUT_CH_PARAM, "MIDI output channel", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 1)
      params:set_action(MIDI_OUT_CH_PARAM, function(idx)
        midi_channel = idx
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,

    div("MIDI In"),
    function()
      params:add_option(MIDI_IN_DEV_PARAM, "MIDI input", midi_in_devices, default_midi_in_index)
      params:set_action(MIDI_IN_DEV_PARAM, function(i) setup_midi_in(i) end)
    end,
    function()
      local ch_opts_in = {"omni"}; for i=1,16 do ch_opts_in[#ch_opts_in+1]=tostring(i) end
      params:add_option(MIDI_IN_CH_PARAM, "chord MIDI input ch", ch_opts_in, 1)
    end,

    div("Instruments (Chord)"),
    function()
      params:add_option("chorder_mx_voice", "mx.samples", (#display_names>0 and display_names or {"(no packs)"}), 1)
      params:set_action("chorder_mx_voice", function(i) voice_index = i; ensure_selected_loaded(); redraw() end)
    end,
    function()
      params:add_trigger("chorder_refresh_voices", "refresh instrument list")
      params:set_action("chorder_refresh_voices", function()
        scan_instruments()
        local p = params:lookup_param("chorder_mx_voice")
        if p then
          p.options = (#display_names>0 and display_names or {"(no packs)"})
          p.count   = (#display_names>0 and #display_names or 1)
          voice_index = 1; params:set("chorder_mx_voice", voice_index)
        end
        -- also refresh free play instrument list
        local pf = params:lookup_param("free_mx_voice")
        if pf then
          pf.options = (#display_names>0 and display_names or {"(no packs)"})
          pf.count   = (#display_names>0 and #display_names or 1)
          free_voice_index = 1; params:set("free_mx_voice", free_voice_index)
        end
        redraw()
      end)
    end,

    div("Levels"),
    function()
      params:add_number("chorder_mx_vol_pct", "mx volume (%)", 0, 200, 100)
      params:set_action("chorder_mx_vol_pct", function(v)
        mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200)
      end)
    end,
  }
  add_section("CHORDER · I/O", g_io)

  -- ========= SECTION 2: CHORDER · Musical Setup =========
  local VOICE_OPTS = {
    "none","drop-2","drop-3","drop-2&4","drop-1",
    "open","wide","quartal","quintal","nearest","smooth"
  }

  local g_setup = {
    div("Key & Scale"),
    function()
      params:add_option("chorder_root_pc", "root pitch", NOTE_NAMES_SHARP, root_pc+1)
      params:set_action("chorder_root_pc", function(i) root_pc = i-1; recompute_root_midi(); redraw() end)
    end,
    function()
      params:add_number("chorder_root_oct", "root octave", -1, 8, root_oct)
      params:set_action("chorder_root_oct", function(v) root_oct = util.clamp(v,-1,8); recompute_root_midi(); redraw() end)
    end,
    function()
      params:add_option("chorder_scale", "scale/mode", SCALE_NAMES, 1)
      params:set_action("chorder_scale", function(i) scale_name = SCALE_NAMES[i] or "Major"; redraw() end)
    end,

    div("Chord Build"),
    function()
      params:add_option("chorder_chord_type", "chord type", {"triad","7th","9th"}, chord_type)
      params:set_action("chorder_chord_type", function(i)
        chord_type = i
        redraw()
      end)
    end,
    function()
      params:add_option("chorder_sus_mode", "third handling", {"normal","sus2","sus4"}, sus_mode)
      params:set_action("chorder_sus_mode", function(i)
        sus_mode = i
        redraw()
      end)
    end,
    function()
      params:add_number("chorder_inversion", "inversion (0-3)", 0, 3, inversion)
      params:set_action("chorder_inversion", function(v) inversion = util.clamp(v,0,3); redraw() end)
    end,
    function()
      params:add_number("chorder_spread", "spread (semitones)", -24, 24, spread_semitones)
      params:set_action("chorder_spread", function(v) spread_semitones = util.round(v); redraw() end)
    end,

    div("Voicing & Bass"),
    function()
      params:add_option("chorder_voicing", "voicing", VOICE_OPTS, voicing_mode)
      params:set_action("chorder_voicing", function(i)
        voicing_mode = i
        show_param("chorder_inversion", voicing_mode ~= 11)
        redraw()
      end)
    end,
    function()
      params:add_option("chorder_bass_note", "add bass (root -12)", {"off","on"}, (add_bass and 2 or 1))
      params:set_action("chorder_bass_note", function(i) add_bass = (i==2); redraw() end)
    end,

    div("Velocity (Chord)"),
    function()
      params:add_option("chorder_vel_mode", "velocity src (chord)", {"fixed","incoming"}, velocity_mode)
      params:set_action("chorder_vel_mode", function(i)
        velocity_mode = i
        show_param("chorder_vel_fixed", i==1)
      end)
    end,
    function()
      params:add_number("chorder_vel_fixed", "fixed velocity (chord)", 1, 127, fixed_velocity)
      params:set_action("chorder_vel_fixed", function(v) fixed_velocity = util.clamp(v,1,127) end)
    end,
  }
  add_section("CHORDER · Musical Setup", g_setup)

  -- ========= SECTION 3: CHORDER · Timing & Feel =========
  local g_timing = {
    div("Clock & Grid"),
    function()
      params:add_option("chorder_quantize", "quantize chords", {"off","on"}, (quantize and 2 or 1))
      params:set_action("chorder_quantize", function(i)
        quantize = (i==2)
        redraw()
      end)
    end,
    function()
      params:add_option("chorder_quant_div", "quantize division", QUANT_DIV_OPTS, 4)
      params:set_action("chorder_quant_div", function(i)
        tick_div_str = QUANT_DIV_OPTS[i]
        tick_div = QUANT_DIV_MAP[tick_div_str] or 1/4
        redraw()
      end)
    end,
    function()
      params:add_option("chorder_swing_mode", "swing mode", {"grid","swing %"}, swing_mode)
      params:set_action("chorder_swing_mode", function(i)
        swing_mode = i
        show_param("chorder_swing_pct", swing_mode == 2)
        redraw()
      end)
    end,
    function()
      params:add_number("chorder_swing_pct", "swing %", 0, 75, swing_percent)
      params:set_action("chorder_swing_pct", function(v) swing_percent = util.clamp(v, 0, 75); redraw() end)
    end,

    -- NEW: explicit Gate param (Chord MIDI) to satisfy show_param + gating
    div("Gate (Chord MIDI)"),
    function()
      params:add_option(MIDI_GATE_PARAM, "MIDI gate", MIDI_GATE_OPTS, 1)
    end,

    div("Strum"),
    function()
      params:add_number("chorder_strum", "strum (steps of division)", 0, 8, strum_steps)
      params:set_action("chorder_strum", function(v) strum_steps = util.clamp(v,0,8); redraw() end)
    end,
    function()
      params:add_option("chorder_strum_type", "strum type", STRUM_OPTS, strum_type)
      params:set_action("chorder_strum_type", function(i)
        strum_type = i
        strum_state = { alt_flip=false, last_first=nil, last_last=nil }
        redraw()
      end)
    end,

    div("Timing Shapes"),
    function()
      params:add_option("chorder_timing_shape", "timing shape",
        {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, 1)
    end,
    function() params:add_number("chorder_timing_amt", "timing amount", 0, 100, 50) end,
    function() params:add_number("chorder_timing_skip_steps", "skip size (steps)", 0, 8, 1) end,

    div("Humanize"),
    function()
      params:add_number("chorder_hum_steps", "humanize timing (max steps)", 0, 4, humanize_steps_max)
      params:set_action("chorder_hum_steps", function(v) humanize_steps_max = util.clamp(v,0,4); redraw() end)
    end,
    function()
      params:add_number("chorder_hum_vel", "humanize velocity (+/-)", 0, 30, humanize_vel_range)
      params:set_action("chorder_hum_vel", function(v) humanize_vel_range = util.clamp(v,0,30); redraw() end)
    end,

    div("Dynamics"),
    function() params:add_number("chorder_ramp_per_step", "velocity ramp / step", -24, 24, -6) end,
    function() params:add_option("chorder_accent", "accent target", {"none","bass","top","middle"}, 2) end,
    function() params:add_number("chorder_accent_amt", "accent amount", -30, 30, 12) end,

    div("Flam"),
    function() params:add_option("chorder_flam", "flam", {"off","on"}, 1) end,
    function() params:add_number("chorder_flam_count", "flam hits (extra)", 0, 3, 1) end,
    function() params:add_number("chorder_flam_space", "flam spacing (steps)", 1, 4, 1) end,
    function() params:add_number("chorder_flam_vel", "flam vel delta", -30, 0, -8) end,
  }
  add_section("CHORDER · Timing & Feel", g_timing)

  -- ========= SECTION 4: CHORDER · Free Play (Chord-enabled) =========
  local g_free = {
    div("Toggle"),
    function()
      params:add_option("free_enable", "free play", {"off","on"}, 1)
      params:set_action("free_enable", function(i)
        free_enable = (i==2)
        if not free_enable then all_free_notes_off() end
        redraw()
      end)
    end,

    div("Instrument & Output"),
    function()
      params:add_option("free_mx_voice", "mx.samples", (#display_names>0 and display_names or {"(no packs)"}), 1)
      params:set_action("free_mx_voice", function(i) free_voice_index = i; free_load_selected(); redraw() end)
    end,
    function()
      params:add_option(FREE_OUT_MODE_PARAM, "output", FREE_OUT_OPTS, 1)
      params:set_action(FREE_OUT_MODE_PARAM, function(_)
        panic_all_outputs()
        rebind_midi_in_if_needed()
        show_param("free_gate_mode", free_want_midi()) -- hide gate unless Free includes MIDI
        redraw()
      end)
    end,

    div("MIDI Out"),
    function()
      params:add_option(FREE_MIDI_OUT_DEV_PARAM, "MIDI out", midi_devices, 1)
      params:set_action(FREE_MIDI_OUT_DEV_PARAM, function(i)
        setup_free_midi_out_awake(i)
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,
    function()
      params:add_option(FREE_MIDI_OUT_CH_PARAM, "MIDI out ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 1)
      params:set_action(FREE_MIDI_OUT_CH_PARAM, function(idx)
        free_midi_channel = idx
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,

    div("MIDI In"),
    function()
      params:add_option(FREE_MIDI_IN_CH_PARAM, "MIDI input ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 2)
    end,

    div("Mode"),
    function()
      params:add_option("free_mode", "play mode", {"mono","chord"}, free_mode)
      params:set_action("free_mode", function(i) free_mode = i end)
    end,

    div("Chord Recipe"),
    function()
      params:add_option("free_seventh", "type", {"triad","7th"}, free_seventh)
      params:set_action("free_seventh", function(i) free_seventh = i end)
    end,
    function()
      params:add_number("free_inversion", "inversion (0-3)", 0, 3, free_inversion)
      params:set_action("free_inversion", function(v) free_inversion = util.clamp(v,0,3) end)
    end,
    function()
      params:add_number("free_spread", "spread (semitones)", -24, 24, free_spread)
      params:set_action("free_spread", function(v) free_spread = util.round(v) end)
    end,
    function()
      params:add_option("free_voicing", "voicing",
        {"none","drop-2","drop-3","drop-2&4","drop-1","open","wide","quartal","quintal","nearest","smooth"}, free_voicing)
      params:set_action("free_voicing", function(i) free_voicing = i end)
    end,
    function()
      params:add_option("free_add_bass", "add bass (root -12)", {"off","on"}, free_add_bass)
      params:set_action("free_add_bass", function(i) free_add_bass = i end)
    end,

    div("Strum (Free)"),
    function()
      params:add_number("free_strum_steps", "strum (steps of division)", 0, 8, free_strum_steps)
      params:set_action("free_strum_steps", function(v) free_strum_steps = util.clamp(v,0,8) end)
    end,
    function()
      params:add_option("free_strum_type", "strum type", STRUM_OPTS, free_strum_type)
      params:set_action("free_strum_type", function(i) free_strum_type = i end)
    end,

    -- NEW: Simple Timing Shapes for Free Play
    div("Timing (Free)"),
    function()
      params:add_option("free_timing_shape", "timing shape",
        {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, free_timing_shape)
      params:set_action("free_timing_shape", function(i) free_timing_shape = i end)
    end,
    function()
      params:add_number("free_timing_amt", "timing amount", 0, 100, free_timing_amt)
      params:set_action("free_timing_amt", function(v) free_timing_amt = util.clamp(v,0,100) end)
    end,
    function()
      params:add_number("free_timing_skip_steps", "skip size (steps)", 0, 8, free_timing_skip_steps)
      params:set_action("free_timing_skip_steps", function(v) free_timing_skip_steps = util.clamp(v,0,8) end)
    end,

    div("Velocity & Gate (Free)"),
    function()
      params:add_option("free_vel_mode", "velocity src", {"fixed","incoming"}, free_velocity_mode)
      params:set_action("free_vel_mode", function(i)
        free_velocity_mode = i
        show_param("free_vel_fixed", i==1)
      end)
    end,
    function()
      params:add_number("free_vel_fixed", "fixed velocity", 1, 127, free_fixed_velocity)
      params:set_action("free_vel_fixed", function(v) free_fixed_velocity = util.clamp(v,1,127) end)
    end,
    function()
      params:add_option("free_gate_mode", "MIDI gate", {"release","25%","50%","75%","100%"}, free_gate_mode)
      params:set_action("free_gate_mode", function(i) free_gate_mode = i end)
    end,

    div("Transpose"),
    function()
      params:add_number("free_transpose_oct", "free play transpose (oct)", -4, 4, 0)
      params:set_action("free_transpose_oct", function(v)
        panic_all_outputs()
        free_transpose_oct = util.clamp(v, -4, 4)
      end)
    end,
  }
  add_section("CHORDER · Free Play", g_free)

  -- MIDI hotplug
  midi.add = function(dev)
    print("MIDI added: "..(dev.name or "?"))
    rebuild_midi_lists(true, true)
    setup_free_midi_out_awake(params:get(FREE_MIDI_OUT_DEV_PARAM) or 1)
  end
  midi.remove = function(dev)
    print("MIDI removed: "..(dev.name or "?"))
    rebuild_midi_lists(true, true)
    setup_midi_in(params:get(MIDI_IN_DEV_PARAM) or default_midi_in_index)
    setup_midi_out_awake(params:get(MIDI_OUT_DEV_PARAM) or 1)
    setup_free_midi_out_awake(params:get(FREE_MIDI_OUT_DEV_PARAM) or 1)
  end

  -- Initial dynamic visibility states (after all params exist)
  show_param("chorder_swing_pct", swing_mode == 2)
  show_param("chorder_vel_fixed", velocity_mode == 1)
  show_param("chorder_inversion", voicing_mode ~= 11)
  show_param(MIDI_GATE_PARAM, want_midi()) -- chord MIDI gate only visible if output includes MIDI
  show_param("free_vel_fixed", free_velocity_mode == 1)
  show_param("free_gate_mode", free_want_midi()) -- Free gate visible only if Free includes MIDI

  -- clock
  recompute_root_midi()
  clock.run(clock_loop)

  ensure_selected_loaded()
  setup_midi_out_awake(1)
  setup_free_midi_out_awake(params:get(FREE_MIDI_OUT_DEV_PARAM) or 1)
  setup_midi_in(params:get(MIDI_IN_DEV_PARAM) or default_midi_in_index)
  rebind_midi_in_if_needed()
  redraw()
end

-- HUD toggle + panic
local k3_down_time = nil
local LONG_PRESS_SEC = 0.35

function key(n, z)
  if n == 2 and z == 1 then
    page = (page == PAGE_OUTPUT) and PAGE_CHORD or PAGE_OUTPUT
    redraw()
  elseif n == 3 then
    if z == 1 then
      k3_down_time = now()
    else
      local held = k3_down_time and (now() - k3_down_time) or 0
      k3_down_time = nil
      if held > LONG_PRESS_SEC then
        page = (page == PAGE_HUD) and PAGE_CHORD or PAGE_HUD
        redraw()
      else
        panic_all_outputs() -- K3 short press: panic
      end
    end
  end
end

function enc(n, d)
  -- E1: toggle between Output and Chord pages
  if n == 1 then
    page = (page == PAGE_OUTPUT) and PAGE_CHORD or PAGE_OUTPUT
    redraw()
    return
  end

  -- Output page encoders
  if page == PAGE_OUTPUT then
    if n == 2 then
      -- change output mode
      params:delta(OUT_MODE_PARAM, d)
    elseif n == 3 then
      -- if mx is in the output path, pick instrument; otherwise pick MIDI out device
      if want_mx() then
        params:delta("chorder_mx_voice", d)
      else
        params:delta(MIDI_OUT_DEV_PARAM, d)
      end
    end
    return
  end

  -- Chord page encoders
  if page == PAGE_CHORD then
    if n == 2 then
      params:delta("chorder_root_pc", d)
    elseif n == 3 then
      params:delta("chorder_scale", d)
    end
    return
  end

  -- HUD page: no encoder actions
end

function cleanup()
  if m then m.event = nil end
  panic_all_outputs()
end
