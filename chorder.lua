-- Chorder - a chord instrument
-- Output: mx.samples and/or MIDI
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
local seventh_mode = false
local inversion = 0
local spread_semitones = 0

-- Voicing:
-- 1=none, 2=drop-2, 3=drop-3, 4=drop-2&4, 5=drop-1, 6=open, 7=wide,
-- 8=quartal, 9=quintal, 10=nearest, 11=smooth
local voicing_mode = 1
local add_bass = false

-- For voice-leading modes
local last_voiced_notes = nil -- ascending MIDI list of last output chord
local last_bass_note = nil

-- Velocity
local velocity_mode = 1 -- 1=fixed, 2=incoming
local fixed_velocity = 100

-- Humanize
local humanize_steps_max = 0
local humanize_vel_range = 0

-- Timing / global clock usage
local quantize = false
local strum_steps = 0

-- Strum type
local STRUM_OPTS = {
  "up","down","up/down","down/up","random",
  "center-out","outside-in","bass-bounce","treble-bounce",
  "random no-repeat first","random stable ends"
}
local strum_type = 1

-- Strum state (internal)
local strum_state = {
  alt_flip = false,      -- for modes 3/4
  last_first = nil,      -- for mode 10/11
  last_last  = nil       -- for mode 11
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
local last_name_timeout = 2.0

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
  screen.level(15); screen.move(4, 12); screen.text("Chorder")
  screen.level(10); screen.move(124, 12); if active_label then screen.text_right("["..active_label.."]") end
end
local function draw_line(y, label, value)
  screen.level(12); screen.move(4, y); screen.text(label or "")
  screen.level(15); screen.move(124, y); screen.text_right(value or "")
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

local function mx_on_safe(canon_name, midi_note, vel127)
  canon_name = sanitize_name(canon_name); if canon_name == "" then return end
  local ok, err = pcall(function()
    mx:on({ name = canon_name, midi = midi_note, velocity = util.clamp(vel127,1,127) })
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

-- Gate metro
local MIDI_GATE_PARAM = "chorder_midi_gate"
local MIDI_GATE_OPTS = {"release","25%","50%","75%","100%"}
local midi_gate_metro = metro.init()

local function compute_gate_seconds()
  local sel = params:get(MIDI_GATE_PARAM) or 1
  if sel == 1 then return nil end
  local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
  local bpm = clock.get_tempo()
  return (60 / bpm) * (tick_div or 1/4) * frac
end

midi_gate_metro.event = function() all_midi_notes_off() end
local function schedule_gate_if_needed()
  local secs = compute_gate_seconds()
  if secs and secs > 0 then midi_gate_metro:start(secs, 1) end
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

-- Helpers for voicing ---------------------------------

local function clone_sorted(t)
  local c = {}
  for i,n in ipairs(t) do c[i]=n end
  table.sort(c)
  return c
end

local function apply_drop(t, which_from_top) -- which_from_top: 1=top, 2=2nd, 3=3rd, 4=4th...
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

local function apply_open_position(t) -- triad: 1-5-3 (3 up); 7th: 1-5-7-3
  local v = clone_sorted(t)
  if #v == 3 then
    -- move the second chord tone (3rd) up
    v = { v[1], v[3], v[2] + 12 }
  elseif #v == 4 then
    v = { v[1], v[3], v[4], v[2] + 12 }
  end
  table.sort(v)
  return v
end

local function apply_wide_spread(t) -- alternate lifting to expand span
  local v = clone_sorted(t)
  for i=2,#v,2 do v[i] = v[i] + 12 end
  table.sort(v)
  return v
end

local function diatonic_stack(sc, i_root, steps, count)
  -- steps = 2 for tertian, 3 for quartal, 4 for quintal
  local idxs = {}
  for k=0,(count-1) do
    idxs[#idxs+1] = i_root + k*steps
  end
  return idxs
end

local function assign_register_nearest(target, prev)
  -- Greedy octave assignment to minimize total |dn| vs previous chord
  -- target: sorted base chord (close voicing) ascending
  -- prev  : last_voiced_notes or nil
  local out = clone_sorted(target)
  if not prev or #prev == 0 or #prev ~= #out then
    -- Center to a reasonable register (~C3..C5)
    local center = 60
    -- Place root near center
    local base = out[1]
    local shift = math.floor((center - base) / 12)
    for i=1,#out do out[i] = out[i] + shift*12 end
    table.sort(out)
    return out
  end

  -- Start by aligning roots near previous bass
  local v = {}
  for i=1,#out do v[i] = out[i] end
  -- Rough-align to previous lowest
  local prev_low = prev[1]
  local base = v[1]
  local shift = math.floor((prev_low - base) / 12)
  for i=1,#v do v[i] = v[i] + shift*12 end

  -- Now nudge each voice by +/- 12 if it reduces motion to the nearest prev voice
  for i=1,#v do
    local cur = v[i]
    -- pair by index (soprano->soprano etc.) for stability
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
  -- Pick inversion 0..3 that minimizes bass movement from last_bass_note
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
  -- Base is sorted close-position notes already built for the chord quality.
  local mode = voicing_mode or 1
  if mode == 1 then
    return notes
  elseif mode == 2 then
    return apply_drop(notes, 2) -- drop-2
  elseif mode == 3 then
    return apply_drop(notes, 3) -- drop-3
  elseif mode == 4 then
    return apply_drop_multiple(notes, {2,4}) -- drop-2&4
  elseif mode == 5 then
    return apply_drop(notes, 1) -- drop-1
  elseif mode == 6 then
    return apply_open_position(notes)
  elseif mode == 7 then
    return apply_wide_spread(notes)
  elseif mode == 8 or mode == 9 then
    -- Rebuild with quartal/quintal stacks
    local steps = (mode == 8) and 3 or 4
    local count = use7 and 4 or 3
    local i_root = base_idxs[1]
    local idxs = diatonic_stack(sc, i_root, steps, count)
    local out = {}
    for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out)
    return out
  elseif mode == 10 then
    -- Nearest voice leading (register assignment)
    return assign_register_nearest(notes, last_voiced_notes)
  elseif mode == 11 then
    -- Smooth inversion: pick inversion with least bass motion, then apply
    -- close voicing at that inversion; allow spread, bass, etc later
    local idxs = choose_smooth_inversion(base_idxs, sc, use7)
    local out = {}
    for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out)
    return out
  end
  return notes
end

-- Return: notes (ascending), quality, name_root_midi
local function chord_for_degree(base_midi, deg, want7th, inv, spread)
  local sc = scale128()
  local i_root = nearest_index_with_degree(sc, base_midi, deg)

  -- Build indexes in tertian by default (we may override inside apply_voicing)
  local idxs = { i_root, i_root+2, i_root+4 }
  if want7th then idxs[#idxs+1] = i_root+6 end

  -- Apply explicit inversion only for modes that don't auto-choose inversion
  local inversion_is_forced = (voicing_mode ~= 11) -- smooth chooses itself
  if inversion_is_forced then
    for _=1, math.min(inv, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
  end

  local notes = {}
  for _,ix in ipairs(idxs) do
    local n = sc[util.clamp(ix, 1, #sc)]
    notes[#notes+1] = n + spread
  end

  local triad3 = {notes[1], notes[2], notes[3]}
  local quality = triad_quality(triad3)
  local name_root_midi = sc[i_root]

  -- Apply voicing algorithm (may rebuild or re-register)
  notes = apply_voicing(notes, idxs, sc, want7th)

  if add_bass then notes[#notes+1] = (notes[1] - 12) end
  table.sort(notes)
  return notes, quality, name_root_midi
end

-- naming
local function base_chord_symbol(root_pc_val, qual, want7th)
  local root_txt = NOTE_NAMES_SHARP[(root_pc_val % 12) + 1]
  if want7th then
    if qual == "maj" then return root_txt .. "maj7" end
    if qual == "min" then return root_txt .. "m7" end
    if qual == "dim" then return root_txt .. "m7b5" end
    if qual == "aug" then return root_txt .. "+7" end
    return root_txt .. "7"
  else
    if qual == "maj" then return root_txt end
    if qual == "min" then return root_txt .. "m" end
    if qual == "dim" then return root_txt .. "dim" end
    if qual == "aug" then return root_txt .. "+" end
    return root_txt
  end
end

local function build_chord_display_name(notes, qual, name_root_midi)
  if type(notes) ~= "table" or #notes == 0 then return "" end
  local root_pc_val = name_root_midi % 12
  local symbol = base_chord_symbol(root_pc_val, qual, seventh_mode)
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

-- ===== event queue / clock =====
local function queue_in_steps(steps, fn) table.insert(evq, {steps=math.max(0, steps), fn=fn}) end
local function schedule(step_steps, fn) if not quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

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
local midi_in_devices = {"none"}        -- display list (e.g., "1: LchkyMini")
local midi_in_ports_map = { [1] = 0 }   -- display index -> vport index (0 = none)
local MIDI_IN_DEV_PARAM = "chorder_midi_in_dev"
local MIDI_IN_CH_PARAM  = "chorder_midi_in_ch"

local function build_midi_in_device_list_awake()
  midi_in_devices = {"none"}
  midi_in_ports_map = { [1] = 0 }
  for i = 1, #midi.vports do
    local long_name  = midi.vports[i].name or ("dev "..i)
    local short_name = (#long_name > 15) and util.acronym(long_name) or long_name
    table.insert(midi_in_devices, (i..": "..short_name))
    midi_in_ports_map[#midi_in_devices] = i
  end
  if #midi_in_devices == 1 then
    -- no devices; leave just "none"
  end
end

local function teardown_midi_in() if m then m.event = nil; m = nil end end

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
    local ch_sel_idx = params:get(MIDI_IN_CH_PARAM)
    if ch_sel_idx ~= 1 then
      local want_ch = ch_sel_idx - 1
      if msg.ch ~= want_ch then return end
    end

    local canon = canonical_names[voice_index] or ""
    local vel = (velocity_mode == 2 and (msg.vel or fixed_velocity) or fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      ensure_selected_loaded()
      local pc = msg.note % 12
      if WHITE_SET[pc] then handle_note_on(canon, msg.note, vel, WHITE_TO_DEG[pc]) end
      redraw()
    elseif (msg.type == "note_off") or (msg.type == "note_on" and msg.vel == 0) then
      local pc = msg.note % 12
      if WHITE_SET[pc] then handle_note_off(canon, msg.note) end
      redraw()
    end
  end

  print("MIDI IN: connected to "..(midi_in_devices[param_index] or ("port "..port)))
end

local function jitter_steps(max_steps) if max_steps <= 0 then return 0 end; return math.random(0, max_steps) end
local function jitter_velocity(vel, range) if range <= 0 then return vel end; local d = math.random(-range, range); return util.clamp(vel + d, 1, 127) end
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

-- Returns (order_table, new_state)
local function make_strum_order_pure(count, stype, state)
  state = state or { alt_flip=false, last_first=nil, last_last=nil }

  -- handle 0/1 safely
  local order = {}
  for i = 1, math.max(count or 0, 0) do order[i] = i end
  if #order <= 1 then return order, state end

  local function center_out(n)
    -- ex: 1..5 -> 3,2,4,1,5 ; 1..4 -> 2,3,1,4
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
      -- even: start at n/2, then n/2+1, then expand
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
    -- ex: 1..5 -> 1,5,2,4,3
    local seq = {}
    local i, j = 1, n
    while i <= j do
      table.insert(seq, i)
      if i ~= j then table.insert(seq, j) end
      i = i + 1; j = j - 1
    end
    return seq
  end

  local function bass_bounce(n)
    -- 1, n, 2, n-1, ...
    return outside_in(n)
  end

  local function treble_bounce(n)
    -- n, 1, n-1, 2, ...
    local seq = outside_in(n)
    reverse_inplace(seq)
    return seq
  end

  local n = #order

  if     stype == 1 then
    -- up
    return order, state

  elseif stype == 2 then
    -- down
    reverse_inplace(order)
    return order, state

  elseif stype == 3 then
    -- alternating up/down
    if state.alt_flip then reverse_inplace(order) end
    state.alt_flip = not state.alt_flip
    return order, state

  elseif stype == 4 then
    -- alternating down/up
    if not state.alt_flip then reverse_inplace(order) end
    state.alt_flip = not state.alt_flip
    return order, state

  elseif stype == 5 then
    -- random
    shuffle_inplace(order)
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 6 then
    -- center-out
    local seq = center_out(n)
    for k=1,n do order[k] = seq[k] end
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 7 then
    -- outside-in
    local seq = outside_in(n)
    for k=1,n do order[k] = seq[k] end
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 8 then
    -- bass-bounce
    local seq = bass_bounce(n)
    for k=1,n do order[k] = seq[k] end
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 9 then
    -- treble-bounce
    local seq = treble_bounce(n)
    for k=1,n do order[k] = seq[k] end
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 10 then
    -- random no-repeat first
    local tries = 0
    repeat
      shuffle_inplace(order)
      tries = tries + 1
    until (order[1] ~= state.last_first) or tries > 8
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state

  elseif stype == 11 then
    -- random stable ends: keep first/last from prior, shuffle middle
    if state.last_first and state.last_last and n >= 3 then
      -- build a middle pool excluding the desired ends; if ends collide, fallback
      local middle = {}
      for i=1,n do
        if i ~= state.last_first and i ~= state.last_last then middle[#middle+1] = i end
      end
      if #middle >= (n-2) then
        shuffle_inplace(middle)
        local out = {}
        out[1] = state.last_first
        for i=1,#middle do out[#out+1] = middle[i] end
        out[#out+1] = state.last_last
        return out, state
      end
    end
    -- first-time fallback: just random, but record ends
    shuffle_inplace(order)
    state.last_first = order[1]
    state.last_last  = order[#order]
    return order, state
  end

  -- default fallback
  return order, state
end

-- Thin wrapper to preserve your existing call sites
local function make_strum_order(count)
  local ord, ns = make_strum_order_pure(count, strum_type, strum_state)
  strum_state = ns
  return ord
end

-- ===== Output mode =====
local OUT_MODE_PARAM = "chorder_out_mode"
local OUT_OPTS = {"mx.samples", "mx.samples + MIDI", "MIDI"}
local function want_mx()   local mval = params:get(OUT_MODE_PARAM) or 1; return (mval==1) or (mval==2) end
local function want_midi() local mval = params:get(OUT_MODE_PARAM) or 1; return (mval==2) or (mval==3) end

local function fanout_note_on(canon, note, vel)
  if want_mx()   then mx_on_safe(canon, note, vel) end
  if want_midi() then
    midi_out_on_awake(note, vel)
    schedule_gate_if_needed()
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

local function handle_note_on(canon, root_note, vel, deg)
  local chord, qual, name_root_midi = chord_for_degree(root_note, deg, seventh_mode, inversion, spread_semitones)

  -- update last-voicing memory for next "nearest/smooth" decisions
  last_voiced_notes = {}
  for i,n in ipairs(chord) do last_voiced_notes[i] = n end
  last_bass_note = chord[1]

  last_chord_name = build_chord_display_name(chord, qual, name_root_midi); last_name_time = now()

  local order = make_strum_order(#chord)
  for k,idx in ipairs(order) do
    local n = chord[idx]
    local s = (k-1) * (strum_steps or 0)
    s = s + jitter_steps(humanize_steps_max)
    local v = jitter_velocity(vel, humanize_vel_range)
    schedule(s, function() fanout_note_on(canon, n, v) end)
  end
end

local function handle_note_off(canon, root_note)
  local pc = root_note % 12
  local deg = WHITE_TO_DEG[pc]
  if not deg then return end
  local chord = chord_for_degree(root_note, deg, seventh_mode, inversion, spread_semitones)
  local notes = (type(chord)=="table" and chord[1] and type(chord[1])=="number") and chord or {}
  for _,n in ipairs(notes) do fanout_note_off(canon, n) end
end

local function setup_midi_in(param_index)
  teardown_midi_in()
  if param_index <= 1 then print("MIDI IN: no input selected"); return end
  local port = param_index - 1
  m = midi.connect(port)
  if not m then print("MIDI IN: connect failed for port "..port); return end

  m.event = function(data)
    local msg = midi.to_msg(data); if not msg then return end
    local ch_sel_idx = params:get(MIDI_IN_CH_PARAM)
    if ch_sel_idx ~= 1 then
      local want_ch = ch_sel_idx - 1
      if msg.ch ~= want_ch then return end
    end

    local canon = canonical_names[voice_index] or ""
    local vel = (velocity_mode == 2 and (msg.vel or fixed_velocity) or fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      ensure_selected_loaded()
      local pc = msg.note % 12
      if WHITE_SET[pc] then handle_note_on(canon, msg.note, vel, WHITE_TO_DEG[pc]) end
      redraw()
    elseif (msg.type == "note_off") or (msg.type == "note_on" and msg.vel == 0) then
      local pc = msg.note % 12
      if WHITE_SET[pc] then handle_note_off(canon, msg.note) end
      redraw()
    end
  end

  print("MIDI IN: connected to "..(midi_in_devices[param_index] or ("port "..(param_index-1))))
end

local function rebind_midi_in_if_needed()
  -- If m is missing or its event handler has been wiped, rebind to current param
  if (m == nil) or (m.event == nil) then
    local cur = params:get(MIDI_IN_DEV_PARAM) or 1
    setup_midi_in(cur)
  end
end

local function rebuild_midi_lists(keep_in, keep_out)
  -- remember current MIDI IN long name (so we can re-select reliably)
  local remembered_in_long = nil
  do
    local cur_idx = params:get(MIDI_IN_DEV_PARAM) or 1
    local vp = midi_in_ports_map[cur_idx]
    if vp and vp > 0 and midi.vports[vp] then
      remembered_in_long = midi.vports[vp].name
    end
  end

  build_midi_device_list_awake()   -- outputs (unchanged)
  build_midi_in_device_list_awake()-- inputs (short names)

  -- refresh IN param options to new short list
  params:hide(MIDI_IN_DEV_PARAM); params:unhide(MIDI_IN_DEV_PARAM)
  local p_in = params:lookup_param(MIDI_IN_DEV_PARAM)
  if p_in then
    p_in.options = midi_in_devices
    p_in.count = #midi_in_devices
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

-- ===== Minimal UI =====
local function key_center_string()
  local name = NOTE_NAMES_SHARP[root_pc+1]
  local rname, roct = midi_to_name_oct(root_midi)
  return string.format("%s (root %s%d)", name, rname, roct)
end

local function draw_output_page()
  draw_header("Output")
  local out_mode = OUT_OPTS[params:get(OUT_MODE_PARAM) or 1] or "?"
  local out_idx = params:get(MIDI_OUT_DEV_PARAM) or 1
  local out_lbl = midi_devices[out_idx] or "—"
  local mo_ch   = tostring(params:get(MIDI_OUT_CH_PARAM) or 1)

  local mi_i    = params:get(MIDI_IN_DEV_PARAM) or 1
  local mi_lbl  = midi_in_devices[mi_i] or "none"
  local ch_in_idx = params:get(MIDI_IN_CH_PARAM) or 1
  local ch_in_disp = (ch_in_idx==1) and "Omni" or ("Ch"..tostring(ch_in_idx-1))

  local gate_txt = MIDI_GATE_OPTS[(params:get(MIDI_GATE_PARAM) or 1)] or "?"
  local bpm = string.format("%d BPM", math.floor(clock.get_tempo() or 120))

  draw_line(28, "Output:", out_mode)
  draw_line(40, "MIDI Out:", ellipsize(out_lbl, 18) .. "  Ch"..mo_ch)
  draw_line(52, "MIDI In:",  ellipsize(mi_lbl, 18) .. "  " .. ch_in_disp)
  screen.level(10); screen.move(4, 64); screen.text("Gate: "..gate_txt.."   Tempo "..bpm)
end


local function draw_chord_page()
  draw_header("Chord")
  draw_line(28, "Key:", key_center_string())
  draw_line(40, "Scale:", scale_name)

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
  local show_name = last_chord_name and ((now_t - last_name_time) < last_name_timeout)
  if show_name then
    screen.level(15)
    if screen.font_size then pcall(function() screen.font_size(12) end) end
    screen.move(64, 34); screen.text_center(ellipsize(last_chord_name, 26))
    if screen.font_size then pcall(function() screen.font_size(8) end) end
  else
    screen.level(10); screen.move(64, 34); screen.text_center("(play a chord)")
  end
  screen.update()
end

function redraw()
  screen.clear()
  if page == PAGE_HUD then draw_hud_page(); return end
  if page == PAGE_OUTPUT then draw_output_page() else draw_chord_page() end
  screen.update()
end

-- ===== lifecycle =====
function init()
  mx = mxsamples:new()
  scan_instruments()
  build_midi_device_list_awake()

  -- param group size must match count below
  local n = 26
  params:add_group("CHORDER", n)

  -- 1 output mode
  params:add_option(OUT_MODE_PARAM, "output", OUT_OPTS, 1)
  params:set_action(OUT_MODE_PARAM, function(_)
    if mx and mx.all_notes_off then pcall(function() mx:all_notes_off() end) end
    all_midi_notes_off()
    rebind_midi_in_if_needed() -- <—— ADD THIS
    redraw()
  end)

  -- 2 instrument
  params:add_option("chorder_mx_voice", "mx.samples inst", (#display_names>0 and display_names or {"(no packs)"}), 1)
  params:set_action("chorder_mx_voice", function(i) voice_index = i; ensure_selected_loaded(); redraw() end)

  -- 3 refresh
  params:add_trigger("chorder_refresh_voices", "refresh instrument list")
  params:set_action("chorder_refresh_voices", function()
    scan_instruments()
    local p = params:lookup_param("chorder_mx_voice")
    if p then
      p.options = (#display_names>0 and display_names or {"(no packs)"})
      p.count   = (#display_names>0 and #display_names or 1)
      voice_index = 1; params:set("chorder_mx_voice", voice_index)
    end
    redraw()
  end)

  -- 4..6 key + mode
  params:add_option("chorder_root_pc", "root pitch", NOTE_NAMES_SHARP, root_pc+1)
  params:set_action("chorder_root_pc", function(i) root_pc = i-1; recompute_root_midi(); redraw() end)
  params:add_number("chorder_root_oct", "root octave", -1, 8, root_oct)
  params:set_action("chorder_root_oct", function(v) root_oct = util.clamp(v,-1,8); recompute_root_midi(); redraw() end)
  params:add_option("chorder_scale", "scale/mode", SCALE_NAMES, 1)
  params:set_action("chorder_scale", function(i) scale_name = SCALE_NAMES[i] or "Major"; redraw() end)

  -- 7..11 chord build
  params:add_option("chorder_seventh", "chord type", {"triad", "7th"}, 1)
  params:set_action("chorder_seventh", function(i) seventh_mode = (i==2); redraw() end)
  params:add_number("chorder_inversion", "inversion (0-3)", 0, 3, inversion)
  params:set_action("chorder_inversion", function(v) inversion = util.clamp(v,0,3); redraw() end)
  params:add_number("chorder_spread", "spread (semitones)", -24, 24, spread_semitones)
  params:set_action("chorder_spread", function(v) spread_semitones = util.round(v); redraw() end)

  -- Expanded voicing list
  local VOICE_OPTS = {
    "none","drop-2","drop-3","drop-2&4","drop-1",
    "open","wide","quartal","quintal","nearest","smooth"
  }
  params:add_option("chorder_voicing", "voicing", VOICE_OPTS, voicing_mode)
  params:set_action("chorder_voicing", function(i) voicing_mode = i; redraw() end)

  params:add_option("chorder_bass_note", "add bass (root -12)", {"off","on"}, (add_bass and 2 or 1))
  params:set_action("chorder_bass_note", function(i) add_bass = (i==2); redraw() end)

  -- 12..13 velocity
  params:add_option("chorder_vel_mode", "velocity source", {"fixed","incoming"}, velocity_mode)
  params:set_action("chorder_vel_mode", function(i) velocity_mode = i end)
  params:add_number("chorder_vel_fixed", "fixed velocity", 1, 127, fixed_velocity)
  params:set_action("chorder_vel_fixed", function(v) fixed_velocity = util.clamp(v,1,127) end)

  -- 14..16 timing
  params:add_option("chorder_quantize", "quantize", {"off","on"}, (quantize and 2 or 1))
  params:set_action("chorder_quantize", function(i) quantize = (i==2); redraw() end)
  params:add_option("chorder_quant_div", "quantize division", QUANT_DIV_OPTS, 3)
  params:set_action("chorder_quant_div", function(i) tick_div_str = QUANT_DIV_OPTS[i]; tick_div = QUANT_DIV_MAP[tick_div_str] or 1/4; redraw() end)
  params:add_number("chorder_strum", "strum (steps of division)", 0, 8, strum_steps)
  params:set_action("chorder_strum", function(v) strum_steps = util.clamp(v,0,8); redraw() end)

  -- 17 strum type
  params:add_option("chorder_strum_type", "strum type", STRUM_OPTS, strum_type)
  params:set_action("chorder_strum_type", function(i)
    strum_type = i
    -- reset state so alternating/random-stable behave predictably after change
    strum_state = { alt_flip=false, last_first=nil, last_last=nil }
    redraw()
  end)


  -- 18..19 humanize
  params:add_number("chorder_hum_steps", "humanize timing (max steps)", 0, 4, humanize_steps_max)
  params:set_action("chorder_hum_steps", function(v) humanize_steps_max = util.clamp(v,0,4); redraw() end)
  params:add_number("chorder_hum_vel", "humanize velocity (+/-)", 0, 30, humanize_vel_range)
  params:set_action("chorder_hum_vel", function(v) humanize_vel_range = util.clamp(v,0,30); redraw() end)

  -- 20..21 swing
  params:add_option("chorder_swing_mode", "swing mode", {"grid","swing %"}, swing_mode)
  params:set_action("chorder_swing_mode", function(i) swing_mode = i; redraw() end)
  params:add_number("chorder_swing_pct", "swing %", 0, 75, swing_percent)
  params:set_action("chorder_swing_pct", function(v) swing_percent = util.clamp(v, 0, 75); redraw() end)

  -- 22..23 MIDI IN
  build_midi_in_device_list_awake()

  -- find default by matching LONG name to an entry's underlying port name
  local function default_midi_in_index_from_long(long_name)
    if not long_name or long_name == "" then return 1 end
    for i = 2, #midi_in_devices do
      local vp = midi_in_ports_map[i]
      local ln = midi.vports[vp] and midi.vports[vp].name
      if ln == long_name then return i end
    end
    return 1
  end

  local default_midi_in_index = default_midi_in_index_from_long(DEFAULT_MIDI_IN_NAME)

  params:add_option(MIDI_IN_DEV_PARAM, "MIDI input", midi_in_devices, default_midi_in_index)
  params:set_action(MIDI_IN_DEV_PARAM, function(i) setup_midi_in(i) end)

  local ch_opts_in = {"omni"}; for i=1,16 do ch_opts_in[#ch_opts_in+1]=tostring(i) end
  params:add_option(MIDI_IN_CH_PARAM, "MIDI input channel", ch_opts_in, 1)

  -- 24 MIDI OUT device
  params:add_option(MIDI_OUT_DEV_PARAM, "MIDI output", midi_devices, 1)
  params:set_action(MIDI_OUT_DEV_PARAM, function(i)
    all_midi_notes_off()
    setup_midi_out_awake(i)
    rebind_midi_in_if_needed() -- <—— ADD THIS
    redraw()
  end)

  -- 25 MIDI OUT channel
  params:add_option(MIDI_OUT_CH_PARAM, "MIDI output channel", (function() local t={}; for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 1)
  params:set_action(MIDI_OUT_CH_PARAM, function(idx)
    midi_channel = idx
    all_midi_notes_off()
    rebind_midi_in_if_needed() -- <—— ADD THIS
    redraw()
  end)

  -- 26 note length gate
  params:add_option(MIDI_GATE_PARAM, "MIDI gate", MIDI_GATE_OPTS, 1)

  -- MIDI hotplug
  midi.add = function(dev)    print("MIDI added: "..(dev.name or "?")); rebuild_midi_lists(true, true) end
  midi.remove = function(dev) print("MIDI removed: "..(dev.name or "?")); rebuild_midi_lists(true, true); setup_midi_in(params:get(MIDI_IN_DEV_PARAM)); setup_midi_out_awake(params:get(MIDI_OUT_DEV_PARAM)) end

  -- clock
  recompute_root_midi()
  clock.run(clock_loop)

  ensure_selected_loaded()
  setup_midi_out_awake(1)
  setup_midi_in(params:get(MIDI_IN_DEV_PARAM) or default_midi_in_index) -- ensure param-selected value wins
  rebind_midi_in_if_needed() -- <—— ADD THIS as a final safety
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
        if mx and mx.all_notes_off then pcall(function() mx:all_notes_off() end) end
        all_midi_notes_off()
      end
    end
  end
end

function enc(n, d)
  if n == 1 then
    page = (page == PAGE_OUTPUT) and PAGE_CHORD or PAGE_OUTPUT
    redraw(); return
  end
  if page == PAGE_OUTPUT then
    if n == 2 then params:delta(OUT_MODE_PARAM, d)
    elseif n == 3 then params:delta(MIDI_GATE_PARAM, d) end
  elseif page == PAGE_CHORD then
    if n == 2 then params:delta("chorder_root_pc", d)
    elseif n == 3 then params:delta("chorder_scale", d) end
  end
end

function cleanup()
  if m then m.event = nil end
  if mx and mx.all_notes_off then pcall(function() mx:all_notes_off() end) end
  all_midi_notes_off()
end
