-- CHORDER — a three-voice chord+based instrument (single-file refactor A+B + Arp MIDI patches)
-- Author: @abhayadana (refactor pass by ChatGPT)
-- Engine: mx.samples and/or MIDI
--
-- Long press K3 for HUD

engine.name = "MxSamples"

local musicutil = require "musicutil"
local mxsamples = include "mx.samples/lib/mx.samples"

-- ===== config =====
local BASE = _path.audio .. "mx.samples"
local DEFAULT_MIDI_IN_NAME  = "Launchkey Mini MK3 1"
local DEFAULT_MIDI_OUT_PORT = 1

-- ===== consolidated state =====
local S = {
  -- UI pages
  PAGE_OUTPUT = 1, PAGE_CHORD = 2, PAGE_HUD = 3, page = 1,

  -- root / scale
  root_pc = 0, root_oct = 1, root_midi = 24, scale_name = "Major",

  -- chord build / voicing
  chord_type = 1, sus_mode = 1, inversion = 0, spread = 0,
  voicing = 1, add_bass = false,

  -- velocity / humanize
  velocity_mode = 1, fixed_velocity = 100,
  humanize_steps_max = 0, humanize_vel_range = 0,

  -- timing / grid
  quantize = false, strum_steps = 0, strum_type = 1,
  tick_div = 1/4, tick_div_str = "1/4", swing_mode = 1, swing_percent = 0, swing_phase = 0,

  -- chord state
  last_voiced_notes = nil, last_bass_note = nil,
  chord_active = {},  -- NEW: per-root record of emitted chord notes

  -- display / hud
  last_chord_name = nil, last_name_time = 0, last_name_timeout = 2.0,

  -- queue & tokens
  evq = {},
  trigger_seq = 0,
  chord_hold_tokens = {},

  -- output mode & levels
  out_mode_param = "chorder_out_mode",
  mx_vol_pct = 100,

  -- MIDI (Chord path)
  midi = {
    devices = {}, ports_map = {}, out = nil, channel = 1,
    in_devices = {"none"}, in_ports_map = { [1] = 0 }, input = nil,
    gate_param = "chorder_midi_gate",
    gate_opts = {"release","25%","50%","75%","100%"},
    out_dev_param = "chorder_midi_out_dev", out_ch_param = "chorder_midi_out_ch",
    in_dev_param = "chorder_midi_in_dev", in_ch_param = "chorder_midi_in_ch",
    active_notes = {},
  },

  -- Instruments (mx.samples)
  display_names = {}, canonical_names = {}, folder_names = {}, voice_index = 1, loaded_set = {},

  -- Free Play
  free = {
    enable=false, voice_index=1,
    out_mode_param = "free_out_mode",
    out_opts = {"mx.samples","mx.samples + MIDI","MIDI"},
    strum_steps=0, strum_type=1, timing_shape=1, timing_amt=50, timing_skip_steps=1,
    velocity_mode=2, fixed_velocity=100, gate_mode=1, midi_channel=1,
    key_mode_param = "free_key_mode", key_mode=1,  -- 1 white; 2 quantized; 3 chromatic mono
    mode=1, seventh=1, inversion=0, spread=0, voicing=1, add_bass=1,
    transpose_oct=0,
    midi_out_dev_param = "free_midi_out_dev", midi_out_ch_param = "free_midi_out_ch",
    midi_in_ch_param  = "free_midi_in_ch",
    mout=nil,
    active_map={}, chord_active={}, hold_tokens={},
    last_name=nil, last_time=0,
    mx_vol_pct = 100,
  },
}

local function next_trigger_id() S.trigger_seq = S.trigger_seq + 1; return S.trigger_seq end

-- ===== constants =====
local K = {
  NOTE_NAMES_SHARP = {"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"},
  STRUM_OPTS = {
    "up","down","up/down","down/up","random",
    "center-out","outside-in","bass-bounce","treble-bounce",
    "random no-repeat first","random stable ends",
    "edge-kiss","ping-pair","arp chunk 2–3","guitar rake","harp gliss split",
    "arp chunk 2–3 ↓","guitar rake ↓","harp gliss split (interleaved)"
  },
  QUANT_DIV_OPTS = {"1/1","1/2","1/3","1/4","1/6","1/8","1/12","1/16","1/24","1/32"},
  QUANT_DIV_MAP  = {["1/1"]=1/1, ["1/2"]=1/2, ["1/3"]=1/3, ["1/4"]=1/4, ["1/6"]=1/6,
                    ["1/8"]=1/8, ["1/12"]=1/12, ["1/16"]=1/16, ["1/24"]=1/24, ["1/32"]=1/32},
  WHITE_SET      = { [0]=true,[2]=true,[4]=true,[5]=true,[7]=true,[9]=true,[11]=true },
  WHITE_TO_DEG   = { [0]=1, [2]=2, [4]=3, [5]=4, [7]=5, [9]=6, [11]=7 },
}

-- ===== small utils =====
local function trim(s) return (s and s:gsub("^%s*(.-)%s*$", "%1")) or "" end
local function lower(s) return string.lower(s or "") end
local function now() return util.time() end
math.randomseed(os.time())

local function recompute_root_midi() S.root_midi = (S.root_oct + 1) * 12 + S.root_pc end
local function midi_to_name_oct(m) local pc=m%12; local oct=math.floor(m/12)-1; return K.NOTE_NAMES_SHARP[pc+1],oct end
local function ellipsize(s, n) s=tostring(s or ""); if #s<=n then return s end; return s:sub(1, math.max(0,n-1)).."…" end
local function short_mode_name(mode) if mode=="mx.samples" then return "mx" elseif mode=="mx.samples + MIDI" then return "mx+M" elseif mode=="MIDI" then return "MIDI" else return mode or "?" end end

-- ===== MX module =====
local MX = (function()
  local function exists(path) local f=io.open(path,"r"); if f then f:close(); return true end; return false end
  local function sanitize_name(s) s = trim(s or ""); s = s:gsub("[/\\]+", ""); return trim(s) end

  local function manifest_name_for_folder(folder)
    local fpath = BASE .. "/" .. folder .. "/manifest.lua"
    if not exists(fpath) then return sanitize_name(folder) end
    local ok, ret = pcall(dofile, fpath)
    if ok and type(ret)=="table" then
      if type(ret.name)=="string" and ret.name~="" then return sanitize_name(ret.name) end
      if ret.info and type(ret.info.name)=="string" then return sanitize_name(ret.info.name) end
      if ret.meta and type(ret.meta.name)=="string" then return sanitize_name(ret.meta.name) end
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
    S.display_names, S.canonical_names, S.folder_names = {}, {}, {}
    for i,rec in ipairs(tmp) do
      S.display_names[i]   = rec.display
      S.canonical_names[i] = rec.canon
      S.folder_names[i]    = rec.folder
    end
    print("Found "..tostring(#S.display_names).." mx.samples instrument(s).")
  end

  local function load_folder_into_helper(folder)
    if not folder or folder=="" then return false end
    if S.loaded_set[lower(folder)] then return true end
    local path = BASE .. "/" .. folder
    local ok = false
    if S.mx and S.mx.load_folder then ok = pcall(function() S.mx:load_folder(path) end)
    elseif S.mx and S.mx.add_folder then ok = pcall(function() S.mx:add_folder(path) end) end
    if ok then S.loaded_set[lower(folder)] = true; print("mx.samples: loaded '"..folder.."'"); return true
    else print("mx.samples: failed to load '"..folder.."' at "..path); return false end
  end

  private_mx_off = function(canon_name, midi_note)
    canon_name = canon_name or ""; if canon_name=="" then return end
    local ok, err = pcall(function() S.mx:off({ name = canon_name, midi = midi_note }) end)
    if not ok then print("mx:off error: "..tostring(err).." (name='"..canon_name.."')") end
  end

  local function ensure_selected_loaded(index)
    local folder = S.folder_names[index or S.voice_index]
    if folder then load_folder_into_helper(folder) end
  end

  local function mx_on_safe_chord(canon_name, midi_note, vel127)
    canon_name = canon_name or ""; if canon_name=="" then return end
    local ok, err = pcall(function()
      S.mx:on({
        name = canon_name, midi = midi_note, velocity = util.clamp(vel127,1,127),
        amp = S.mx_vol_pct / 100, gain = S.mx_vol_pct / 100
      })
    end)
    if not ok then print("mx:on error: "..tostring(err).." (name='"..canon_name.."')") end
  end

  local function mx_on_safe_free(canon_name, midi_note, vel127)
    canon_name = canon_name or ""; if canon_name=="" then return end
    local ok, err = pcall(function()
      S.mx:on({
        name = canon_name, midi = midi_note, velocity = util.clamp(vel127,1,127),
        amp = S.free.mx_vol_pct / 100, gain = S.free.mx_vol_pct / 100
      })
    end)
    if not ok then print("mx:on error: "..tostring(err).." (name='"..canon_name.."')") end
  end

  return {
    scan = scan_instruments,
    ensure_loaded = ensure_selected_loaded,
    load_folder = load_folder_into_helper,
    on_chord = mx_on_safe_chord,
    on_free  = mx_on_safe_free,
    off      = private_mx_off,
  }
end)()

-- ===== MIDI module (Awake-style) =====
local MIDI = (function()
  local function active_clear() S.midi.active_notes = {} end
  local function active_on(n) S.midi.active_notes[n] = true end
  private_active_off = function(n) S.midi.active_notes[n] = nil end

  local function build_midi_device_list_awake()
    S.midi.devices, S.midi.ports_map = {}, {}
    for i = 1, #midi.vports do
      local long_name = midi.vports[i].name or ("dev "..i)
      local short_name = (#long_name > 15) and util.acronym(long_name) or long_name
      table.insert(S.midi.devices, (i..": "..short_name))
      S.midi.ports_map[#S.midi.devices] = i
    end
    if #S.midi.devices == 0 then
      table.insert(S.midi.devices, "1: (no devices)")
      S.midi.ports_map[1] = 1
    end
  end

  local function build_midi_in_device_list_awake()
    S.midi.in_devices = {"none"}
    S.midi.in_ports_map = { [1] = 0 }
    for i = 1, #midi.vports do
      local long_name  = midi.vports[i].name or ("dev "..i)
      local short_name = (#long_name > 15) and util.acronym(long_name) or long_name
      table.insert(S.midi.in_devices, (i..": "..short_name))
      S.midi.in_ports_map[#S.midi.in_devices] = i
    end
  end

  local function setup_midi_out_awake(param_index)
    local real_port = S.midi.ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
    if S.midi.out then S.midi.out.event = nil; S.midi.out = nil end
    S.midi.out = midi.connect(real_port)
    if S.midi.out then print("MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
    else print("MIDI OUT: connect failed for port "..real_port) end
  end

  local function midi_out_on_awake(note, vel127)
    if not S.midi.out then return end
    local vel = util.clamp(vel127, 1, 127)
    local n   = util.clamp(note,   0, 127)
    local ok, err = pcall(function() S.midi.out:note_on(n, vel, S.midi.channel) end)
    if not ok then print("MIDI OUT note_on error: "..tostring(err)) end
    active_on(n)
  end

  local function all_midi_notes_off()
    if not S.midi.out then active_clear(); return end
    for n,_ in pairs(S.midi.active_notes) do pcall(function() S.midi.out:note_off(n, 0, S.midi.channel) end) end
    active_clear()
  end

  local function compute_gate_seconds()
    local sel = params:get(S.midi.gate_param) or 1
    if sel == 1 then return nil end
    local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
    local bpm = clock.get_tempo()
    return (60 / bpm) * (S.tick_div or 1/4) * frac
  end

  local function schedule_gate_for_note(note)
    local secs = compute_gate_seconds()
    if not secs or secs <= 0 then return end
    local n = util.clamp(note, 0, 127)
    clock.run(function()
      clock.sleep(secs)
      if S.midi.active_notes[n] then
        pcall(function() if S.midi.out then S.midi.out:note_off(n, 0, S.midi.channel) end end)
        private_active_off(n)
      end
    end)
  end

  return {
    build_out = build_midi_device_list_awake,
    build_in  = build_midi_in_device_list_awake,
    setup_out = setup_midi_out_awake,
    on        = midi_out_on_awake,
    off_all   = all_midi_notes_off,
    gate_for  = schedule_gate_for_note,
  }
end)()

-- ===== chord math & voicing =====
local function scale128() return musicutil.generate_scale_of_length(S.root_midi, S.scale_name, 128) end

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

local function clone_sorted(t) local c = {}; for i,n in ipairs(t) do c[i]=n end; table.sort(c); return c end
local function apply_drop(t, which_from_top) local v = clone_sorted(t); if #v < which_from_top then return v end; local idx = #v - which_from_top + 1; v[idx] = v[idx] - 12; table.sort(v); return v end
local function apply_drop_multiple(t, list_from_top) local v = clone_sorted(t); table.sort(list_from_top); for i=#list_from_top,1,-1 do v = apply_drop(v, list_from_top[i]) end; return v end
local function apply_open_position(t) local v = clone_sorted(t); if #v == 3 then v = { v[1], v[3], v[2] + 12 } elseif #v == 4 then v = { v[1], v[3], v[4], v[2] + 12 } end; table.sort(v); return v end
local function apply_wide_spread(t) local v = clone_sorted(t); for i=2,#v,2 do v[i] = v[i] + 12 end; table.sort(v); return v end
local function diatonic_stack(sc, i_root, steps, count) local idxs = {}; for k=0,(count-1) do idxs[#idxs+1] = i_root + k*steps end; return idxs end

local function assign_register_nearest(target, prev)
  local out = clone_sorted(target)
  if not prev or #prev == 0 or #prev ~= #out then
    local center = 60
    local base = out[1]
    local shift = math.floor((center - base) / 12)
    for i=1,#out do out[i] = out[i] + shift*12 end
    table.sort(out); return out
  end
  local v = {}; for i=1,#out do v[i] = out[i] end
  local prev_low = prev[1]; local base = v[1]
  local shift = math.floor(prev_low - base) // 12
  for i=1,#v do v[i] = v[i] + shift*12 end
  for i=1,#v do
    local cur = v[i]; local want = prev[i]
    local best = cur; local bestd = math.abs(cur - want)
    local a = cur - 12; local ad = math.abs(a - want); if ad < bestd then best, bestd = a, ad end
    local b = cur + 12; local bd = math.abs(b - want); if bd < bestd then best, bestd = b, bd end
    v[i] = best
  end
  table.sort(v); return v
end

local function choose_smooth_inversion(base_idxs, sc, want7)
  if not S.last_bass_note then return base_idxs end
  local best_idxs, best = base_idxs, nil
  local max_inv = math.max(0, (want7 and 3 or 2))
  for inv=0,max_inv do
    local idxs = {}
    for _,ix in ipairs(base_idxs) do idxs[#idxs+1] = ix end
    for _=1, math.min(inv, #idxs-1) do local n = table.remove(idxs, 1); table.insert(idxs, n + 7) end
    local n0 = sc[util.clamp(idxs[1], 1, #sc)]
    local d = math.abs(n0 - S.last_bass_note)
    if not best or d < best then best = d; best_idxs = idxs end
  end
  return best_idxs
end

local function apply_voicing(notes, base_idxs, sc, use7)
  local mode = S.voicing or 1
  if     mode == 1 then return notes
  elseif mode == 2 then return apply_drop(notes, 2)
  elseif mode == 3 then return apply_drop(notes, 3)
  elseif mode == 4 then return apply_drop_multiple(notes, {2,4})
  elseif mode == 5 then return apply_drop(notes, 1)
  elseif mode == 6 then return apply_open_position(notes)
  elseif mode == 7 then return apply_wide_spread(notes)
  elseif mode == 8 or mode == 9 then
    local steps = (mode == 8) and 3 or 4
    local count = use7 and 4 or 3
    local i_root = base_idxs[1]
    local idxs = diatonic_stack(sc, i_root, steps, count)
    local out = {}; for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out); return out
  elseif mode == 10 then return assign_register_nearest(notes, S.last_voiced_notes)
  elseif mode == 11 then
    local idxs = choose_smooth_inversion(base_idxs, sc, use7)
    local out = {}; for _,ix in ipairs(idxs) do out[#out+1] = sc[util.clamp(ix,1,#sc)] end
    table.sort(out); return out
  end
  return notes
end

-- Build chord tones, return (notes ascending, quality, naming_root_midi)
local function chord_for_degree(base_midi, deg, chord_type_sel, inv, spread, sus_sel)
  local sc = scale128()
  local i_root = nearest_index_with_degree(sc, base_midi, deg)

  local idxs = { i_root, i_root+2, i_root+4 }
  local want7 = (chord_type_sel >= 2)
  local want9 = (chord_type_sel >= 3)
  if want7 then idxs[#idxs+1] = i_root+6 end
  if want9 then idxs[#idxs+1] = i_root+8 end

  if sus_sel and sus_sel ~= 1 then
    for k = 1, #idxs do
      if idxs[k] == i_root+2 then
        if     sus_sel == 2 then idxs[k] = i_root+1
        elseif sus_sel == 3 then idxs[k] = i_root+3 end
        break
      end
    end
  end

  local inversion_is_forced = (S.voicing ~= 11)
  if inversion_is_forced then
    for _ = 1, math.min(inv or 0, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
  end

  local notes = {}
  for _,ix in ipairs(idxs) do
    local n = sc[util.clamp(ix, 1, #sc)]
    notes[#notes+1] = n + (spread or 0)
  end

  local triad3 = {notes[1], notes[2], notes[3]}
  local quality = triad_quality(triad3)
  local name_root_midi = sc[i_root]

  notes = apply_voicing(notes, idxs, sc, want7)
  if S.add_bass then notes[#notes+1] = (notes[1] - 12) end
  table.sort(notes)
  return notes, quality, name_root_midi
end

local function base_chord_symbol(root_pc_val, qual, chord_type_sel, sus_sel)
  local root_txt = K.NOTE_NAMES_SHARP[(root_pc_val % 12) + 1]
  local sus_suffix = (sus_sel == 2 and "sus2") or (sus_sel == 3 and "sus4") or nil
  if sus_suffix then
    if     chord_type_sel == 1 then return root_txt .. sus_suffix
    elseif chord_type_sel == 2 then return root_txt .. "7"  .. sus_suffix
    elseif chord_type_sel == 3 then return root_txt .. "9"  .. sus_suffix end
    return root_txt .. sus_suffix
  else
    if     chord_type_sel == 3 then
      if qual == "maj" then return root_txt .. "maj9"
      elseif qual == "min" then return root_txt .. "m9"
      elseif qual == "dim" then return root_txt .. "m9b5"
      elseif qual == "aug" then return root_txt .. "+9" end
      return root_txt .. "9"
    elseif chord_type_sel == 2 then
      if qual == "maj" then return root_txt .. "maj7"
      elseif qual == "min" then return root_txt .. "m7"
      elseif qual == "dim" then return root_txt .. "m7b5"
      elseif qual == "aug" then return root_txt .. "+7" end
      return root_txt .. "7"
    else
      if qual == "maj" then return root_txt
      elseif qual == "min" then return root_txt .. "m"
      elseif qual == "dim" then return root_txt .. "dim"
      elseif qual == "aug" then return root_txt .. "+" end
      return root_txt
    end
  end
end

local function build_chord_display_name(notes, qual, name_root_midi)
  if type(notes) ~= "table" or #notes == 0 then return "" end
  local root_pc_val = name_root_midi % 12
  local symbol = base_chord_symbol(root_pc_val, qual, S.chord_type, S.sus_mode)
  local bass = notes[1]; for i=2,#notes do if notes[i] < bass then bass = notes[i] end end
  local bass_pc = bass % 12
  if bass_pc ~= root_pc_val then symbol = symbol .. "/" .. K.NOTE_NAMES_SHARP[bass_pc + 1] end
  local tags = {}
  if S.inversion > 0 and (S.voicing ~= 11) then table.insert(tags, "inv"..S.inversion) end
  if     S.voicing == 2 then table.insert(tags, "drop2")
  elseif S.voicing == 3 then table.insert(tags, "drop3")
  elseif S.voicing == 4 then table.insert(tags, "drop2&4")
  elseif S.voicing == 5 then table.insert(tags, "drop1")
  elseif S.voicing == 6 then table.insert(tags, "open")
  elseif S.voicing == 7 then table.insert(tags, "wide")
  elseif S.voicing == 8 then table.insert(tags, "quartal")
  elseif S.voicing == 9 then table.insert(tags, "quintal")
  elseif S.voicing == 10 then table.insert(tags, "nearest")
  elseif S.voicing == 11 then table.insert(tags, "smooth") end
  if S.add_bass and bass_pc == root_pc_val then table.insert(tags, "bass") end
  if S.spread ~= 0 then
    local s = (S.spread > 0) and ("+"..S.spread) or tostring(S.spread)
    table.insert(tags, "spread"..s)
  end
  if #tags > 0 then symbol = symbol .. " (" .. table.concat(tags, ", ") .. ")" end
  return symbol
end

-- ===== Strum module =====
local Strum = (function()
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

  local function center_out(n)
    local seq = {}
    local lo = math.floor((n + 1) / 2)
    local hi = lo + 1
    if n % 2 == 1 then
      table.insert(seq, lo)
      while (#seq < n) do
        if hi <= n then table.insert(seq, hi) end
        local left = lo - ((#seq % 2 == 0) and 0 or 1)
        if left >= 1 and #seq < n then table.insert(seq, left) end
        hi = hi + 1
      end
    else
      local a, b = n / 2, n / 2 + 1
      table.insert(seq, a)
      table.insert(seq, b)
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
      i = i + 1
      j = j - 1
    end
    return seq
  end

  local function edge_kiss(n) return outside_in(n) end

  local function ping_pair(n)
    local seq = {}
    local L, R = 1, n
    while L < R do
      table.insert(seq, L)
      if L + 1 <= R then table.insert(seq, L + 1) end
      if R - 1 >= L + 2 then table.insert(seq, R - 1) end
      table.insert(seq, R)
      L = L + 2
      R = R - 2
    end
    if L == R then table.insert(seq, L) end
    return seq
  end

  local function arp_chunk_2_3(n, up)
    local base = {}
    if up then for i = 1, n do base[#base + 1] = i end
    else for i = n, 1, -1 do base[#base + 1] = i end end
    local seq, i, toggle = {}, 1, true
    while i <= #base do
      local sz = toggle and 2 or 3
      for k = i, math.min(i + sz - 1, #base) do seq[#seq + 1] = base[k] end
      i = i + sz
      toggle = not toggle
    end
    return seq
  end

  local function guitar_rake(n, up)
    local seq = {}
    if up then for i = 1, n do seq[#seq + 1] = i end
    else for i = n, 1, -1 do seq[#seq + 1] = i end end
    return seq
  end

  local function harp_gliss_split(n)
    local mid = math.floor(n / 2)
    local lo = {}; for i = 1, mid do lo[#lo + 1] = i end
    local hi = {}; for i = mid + 1, n do hi[#hi + 1] = i end
    local seq = {}
    for i = 1, #lo do seq[#seq + 1] = lo[i] end
    for i = 1, #hi do seq[#seq + 1] = hi[i] end
    return seq
  end

  local function harp_gliss_split_interleaved(n)
    local mid = math.floor(n / 2)
    local lo, hi = {}, {}
    for i = 1, mid do lo[#lo + 1] = i end
    for i = mid + 1, n do hi[#hi + 1] = i end
    local seq, i = {}, 1
    while i <= math.max(#lo, #hi) do
      if i <= #lo then seq[#seq + 1] = lo[i] end
      if i <= #hi then seq[#seq + 1] = hi[i] end
      i = i + 1
    end
    return seq
  end

  local function make(count, stype, state)
    state = state or { alt_flip = false, last_first = nil, last_last = nil }
    local order = {}
    for i = 1, math.max(count or 0, 0) do order[i] = i end
    local n = #order
    if n <= 1 then return order, state end

    if     stype == 1  then return order, state
    elseif stype == 2  then reverse_inplace(order); return order, state
    elseif stype == 3  then if state.alt_flip then reverse_inplace(order) end; state.alt_flip = not state.alt_flip; return order, state
    elseif stype == 4  then if not state.alt_flip then reverse_inplace(order) end; state.alt_flip = not state.alt_flip; return order, state
    elseif stype == 5  then shuffle_inplace(order); state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 6  then local seq = center_out(n); for k = 1, n do order[k] = seq[k] end; state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 7  then local seq = outside_in(n); for k = 1, n do order[k] = seq[k] end; state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 8  then local seq = outside_in(n); for k = 1, n do order[k] = seq[k] end; state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 9  then local seq = outside_in(n); reverse_inplace(seq); for k = 1, n do order[k] = seq[k] end; state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 10 then local tries = 0; repeat shuffle_inplace(order); tries = tries + 1 until (order[1] ~= state.last_first) or tries > 8; state.last_first = order[1]; state.last_last  = order[#order]; return order, state
    elseif stype == 11 then
      if state.last_first and state.last_last and n >= 3 then
        local middle = {}
        for i = 1, n do if i ~= state.last_first and i ~= state.last_last then middle[#middle + 1] = i end end
        shuffle_inplace(middle)
        local out = { state.last_first }
        for i = 1, #middle do out[#out + 1] = middle[i] end
        out[#out + 1] = state.last_last
        return out, state
      end
      shuffle_inplace(order)
      state.last_first = order[1]
      state.last_last  = order[#order]
      return order, state
    elseif stype == 12 then return edge_kiss(n), state
    elseif stype == 13 then return ping_pair(n), state
    elseif stype == 14 then return arp_chunk_2_3(n, true), state
    elseif stype == 15 then return guitar_rake(n, true), state
    elseif stype == 16 then return harp_gliss_split(n), state
    elseif stype == 17 then return arp_chunk_2_3(n, false), state
    elseif stype == 18 then return guitar_rake(n, false), state
    elseif stype == 19 then return harp_gliss_split_interleaved(n), state
    end

    return order, state
  end

  return { make = make }
end)()

-- ===== Arp module =====
local Arp = (function()
  local A = {
    running = false,
    ord_state = { alt_flip=false, last_first=nil, last_last=nil },
    step_i = 1,
    notes_buf = {},            -- one-octave material, ascending
    emitted = {},              -- active note-ons
    coro = nil,
    hold_count = 0,            -- for key-held mode
    mout = nil,                -- dedicated MIDI out (can be nil to use chord OUT)
  }

  -- --- Output routing (own device/voice) ---
  local function want_mx()
    local mval = params:get("arp_out_mode") or 1
    return (mval==1) or (mval==2)
  end
  local function want_midi()
    local mval = params:get("arp_out_mode") or 1
    return (mval==2) or (mval==3)
  end

  local function setup_midi_out(param_index_in_s_midi_devices)
    -- param_index_in_s_midi_devices is 1..#S.midi.devices (no sentinel)
    local real_port = S.midi.ports_map[param_index_in_s_midi_devices] or 1
    if A.mout then A.mout.event = nil; A.mout = nil end
    A.mout = midi.connect(real_port)
    if A.mout then print("ARP MIDI OUT: "..(midi.vports[real_port].name or ("port "..real_port))) end
  end

  -- SMART FANOUT with fallback: if MIDI requested but unavailable, still monitor via mx
  local function fanout_on(note, vel)
    local canon = S.canonical_names[params:get("arp_mx_voice") or 1] or ""
    -- send MIDI if requested and available
    local sent_midi = false
    if want_midi() then
      local ch = params:get("arp_midi_out_ch") or 1
      local dev = A.mout or S.midi.out  -- fallback to chord MIDI out when Arp device sentinel is selected
      if not dev then print("ARP: want_midi() but no MIDI device (A.mout=nil and S.midi.out=nil)") end
      print(string.format("ARP SEND on: note=%d vel=%d ch=%d midi=%s", note, vel, ch, dev and "yes" or "no"))
      if dev then
        pcall(function() dev:note_on(util.clamp(note,0,127), util.clamp(vel,1,127), ch) end)
        sent_midi = true
      end
    end
    -- smart fallback monitor to mx if (a) mx is enabled OR (b) MIDI-only was requested but nothing actually sent
    if want_mx() or (want_midi() and not sent_midi) then
      MX.on_free(canon, note, util.clamp(vel,1,127))
    end
  end
  local function fanout_off(note)
    local canon = S.canonical_names[params:get("arp_mx_voice") or 1] or ""
    local turned_off_midi = false
    if want_midi() then
      local ch = params:get("arp_midi_out_ch") or 1
      local dev = A.mout or S.midi.out
      print(string.format("ARP SEND off: note=%d ch=%d midi=%s", note, ch, dev and "yes" or "no"))
      if dev then
        pcall(function() dev:note_off(util.clamp(note,0,127), 0, ch) end)
        turned_off_midi = true
      end
    end
    if want_mx() or (want_midi() and not turned_off_midi) then
      MX.off(canon, note)
    end
  end
  local function all_off() for n,_ in pairs(A.emitted) do fanout_off(n) end; A.emitted = {} end

  -- --- Material (chord tones + optional passing) ---
  local function scale_between(a,b, sc)
    local lo, hi = math.min(a,b), math.max(a,b)
    local out = {}
    for i=1,#sc do local v = sc[i]; if v>lo and v<hi then out[#out+1]=v end end
    table.sort(out); return out
  end

  local function make_material()
    -- ENFORCEMENT: do not auto-generate arp material until a chord has been played
    local base_notes = {}
    if type(S.last_voiced_notes)=="table" and #S.last_voiced_notes>0 then
      for _,n in ipairs(S.last_voiced_notes) do base_notes[#base_notes+1]=n end
    else
      -- No chord captured yet -> keep material empty so arp stays silent
      A.notes_buf = {}
      return
    end
    table.sort(base_notes)

    local mode = params:get("arp_material") or 2 -- 1=chord only, 2=chord + passing (default)
    if mode == 1 then A.notes_buf = base_notes; return end

    local sc = scale128()
    local buf = {}
    for i=1,#base_notes do
      local n = base_notes[i]
      buf[#buf+1] = n
      if i < #base_notes then
        local mids = scale_between(n, base_notes[i+1], sc)
        for _,m in ipairs(mids) do buf[#buf+1] = m end
      end
    end
    table.sort(buf)
    A.notes_buf = buf
  end

  -- --- Order / walk ---
  local function build_order()
    local stype = params:get("arp_order") or 1
    return select(1, Strum.make(#A.notes_buf, stype, A.ord_state)) or {}
  end
  local function next_note(step_idx)
    local ord = build_order(); if #ord==0 then return nil end
    local idx = ord[((step_idx-1) % #ord) + 1]
    local base = A.notes_buf[idx]
    local span = params:get("arp_octaves") or 1
    local walk = params:get("arp_oct_walk") or 1 -- 1=wrap (default), 2=bounce
    local t = math.floor((step_idx-1)/#ord)
    local o
    if span<=0 then o=0
    else
      if walk==2 then
        local period = span*2
        local p = (period==0) and 0 or (t % period)
        o = (p<=span) and p or (period - p)
      else
        o = t % (span+1) -- wrap
      end
    end
    return util.clamp(base + o*12, 0, 127)
  end

  -- --- Timing / gate / swing (separate from chord) ---
  local function step_seconds()
    local div = K.QUANT_DIV_MAP[K.QUANT_DIV_OPTS[params:get("arp_div") or 6] or "1/8"] or 1/8
    local bpm = clock.get_tempo()
    local base = (60/bpm)*div
    local sm  = params:get("arp_swing_mode") or 1
    local pct = (params:get("arp_swing_pct") or 0)/100
    if sm==2 and pct>0 then
      if (A.step_i % 2)==1 then return base*(1+pct) else return base*(1-pct) end
    end
    return base
  end

  local function step_once()
    all_off()
    if not A.running then return end
    local vel = util.clamp((params:get("arp_vel") or 100) + (A.step_i-1)*(params:get("arp_vel_ramp") or 0), 1, 127)
    local n = next_note(A.step_i)
    if n then
      fanout_on(n, vel)
      A.emitted[n]=true
      local gm = params:get("arp_gate") or 1
      if gm > 1 then
        local frac = (gm==2 and 0.25) or (gm==3 and 0.50) or (gm==4 and 0.75) or 1.0
        local dur = step_seconds()*frac
        clock.run(function() clock.sleep(dur); if A.emitted[n] then fanout_off(n); A.emitted[n]=nil end end)
      end
    end
    A.step_i = A.step_i + 1
  end

  -- --- Lifecycle ---
  local function run()
    A.running = true
    A.step_i  = 1
    make_material()
    print("ARP: start")
    while A.running do
      local wait = step_seconds()
      step_once()
      clock.sleep(wait)
      if params:get("arp_retrack")==2 and ((A.step_i-1) % math.max(#A.notes_buf,1) == 0) then
        make_material()
      end
    end
  end

  local function start()
    if A.running then return end
    if (params:get("arp_enable") or 1) ~= 2 then return end
    if A.coro then clock.cancel(A.coro) end
    A.coro = clock.run(run)
  end
  local function stop()
    if not A.running and not A.coro then return end
    A.running = false
    if A.coro then clock.cancel(A.coro); A.coro=nil end
    print("ARP: stop")
    all_off()
  end

  -- chord-key notifications (for key-held mode or latch restart)
  local function chord_key(down)
    local mode = params:get("arp_trigger_mode") or 1 -- 1=key-held, 2=latch
    if mode==2 then
      if down then make_material(); A.step_i=1; start() end
      return
    end
    -- key-held:
    if down then
      A.hold_count = A.hold_count + 1
      make_material(); A.step_i=1
      start()
    else
      A.hold_count = math.max(0, A.hold_count - 1)
      if A.hold_count==0 then stop() end
    end
  end

  local function hotplug_refresh()
    local sel = params:get("arp_midi_out_dev") or 1
    -- sel==1 is sentinel "(use Chord MIDI out)"
    if sel == 1 then
      A.mout = nil
      print("ARP MIDI OUT: using Chord MIDI out (sentinel)")
    else
      setup_midi_out(sel - 1)
    end
  end

  return {
    start=start, stop=stop, refresh=make_material, chord_key=chord_key,
    setup_midi_out=setup_midi_out, hotplug_refresh=hotplug_refresh
  }
end)()

-- ===== Chord timing & shaping =====
local function compute_step_offsets(m)
  local base = math.max(0, S.strum_steps or 0)
  if m <= 1 or base == 0 then local offs = {}; for k=1,m do offs[k-1] = 0 end; return offs end

  local shape = params:get("chorder_timing_shape") or 1
  local amt   = (params:get("chorder_timing_amt") or 50) / 100.0
  local spacing = {}; for k=1,m-1 do spacing[k] = base end

  local function apply_serpentine()
    local mid = (m+1)/2
    for k=1,m-1 do local d = math.abs((k - mid) / mid); spacing[k] = base * (1 + amt * d) end
  end
  local function apply_accel()
    for k=1,m-1 do local t = (k-1)/math.max(1,(m-2)); spacing[k] = math.max(0.1, base * (1 - amt * t)) end
  end
  local function apply_rit()
    for k=1,m-1 do local t = (k-1)/math.max(1,(m-2)); spacing[k] = base * (1 + amt * t) end
  end
  local function ease_in_quad(t)  return t*t end
  local function ease_out_quad(t) return 1 - (1-t)*(1-t) end
  local function apply_rake(ease_fn)
    for k=1,m-1 do
      local t = (k-1)/math.max(1,(m-2)); local w = ease_fn(t)
      spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5)*2))
      spacing[k] = math.max(0.1, spacing[k])
    end
  end
  local function apply_skip_alt()
    local add = params:get("chorder_timing_skip_steps") or 1
    local add_eff = add * amt
    for k=1,m-1 do if (k % 2) == 1 then spacing[k] = spacing[k] + add_eff end end
  end

  if     shape == 2 then apply_serpentine()
  elseif shape == 3 then apply_accel()
  elseif shape == 4 then apply_rit()
  elseif shape == 5 then apply_rake(ease_in_quad)
  elseif shape == 6 then apply_rake(ease_out_quad)
  elseif shape == 7 then apply_skip_alt() end

  local offs = {}; local sum = 0; offs[0] = 0
  for k=1,m-1 do sum = sum + spacing[k]; offs[k] = util.round(sum) end
  return offs
end

local function apply_velocity_profile(k, m, base_vel, note_idx_in_sorted, n_sorted)
  local v = base_vel
  v = v + (k-1) * (params:get("chorder_ramp_per_step") or 0)
  local acc_tgt = params:get("chorder_accent") or 1
  local acc_amt = params:get("chorder_accent_amt") or 0
  if     acc_tgt == 2 then if note_idx_in_sorted == 1 then v = v + acc_amt end
  elseif acc_tgt == 3 then if note_idx_in_sorted == n_sorted then v = v + acc_amt end
  elseif acc_tgt == 4 then local mid = math.ceil(n_sorted/2); if note_idx_in_sorted == mid then v = v + acc_amt end end
  return util.clamp(math.floor(v), 1, 127)
end

-- ===== Output helpers (Chord) =====
local OUT_OPTS = {"mx.samples", "mx.samples + MIDI", "MIDI"}
local function want_mx()   local mval = params:get(S.out_mode_param) or 1; return (mval==1) or (mval==2) end
local function want_midi() local mval = params:get(S.out_mode_param) or 1; return (mval==2) or (mval==3) end
local function mx_scaled_vel(v) local scaled = math.floor((v or 100) * (S.mx_vol_pct / 100)); return util.clamp(scaled, 1, 127) end

local function fanout_note_on(canon, note, vel)
  if want_mx()   then MX.on_chord(canon, note, mx_scaled_vel(vel)) end
  if want_midi() then MIDI.on(note, vel); MIDI.gate_for(note) end
end
local function fanout_note_off(canon, note)
  if want_mx()   then MX.off(canon, note) end
  if want_midi() then pcall(function() if S.midi.out then S.midi.out:note_off(note, 0, S.midi.channel) end end) end
end

local function jitter_steps(max_steps) if max_steps <= 0 then return 0 end; return math.random(0, max_steps) end
local function jitter_velocity(vel, range) if range <= 0 then return vel end; local d = math.random(-range, range); return util.clamp(vel + d, 1, 127) end

-- ===== Free module (key modes, timing, fanout) =====
local Free = (function()
  local function free_want_mx()
    local mval = params:get(S.free.out_mode_param) or 1
    return (mval==1) or (mval==2)
  end
  local function free_want_midi()
    local mval = params:get(S.free.out_mode_param) or 1
    return (mval==2) or (mval==3)
  end

  local function free_scaled_vel(v)
    local scaled = math.floor((v or 100) * (S.free.mx_vol_pct / 100))
    return util.clamp(scaled, 1, 127)
  end

  local function setup_free_midi_out_awake(param_index)
    local real_port = S.midi.ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
    if S.free.mout then S.free.mout.event = nil; S.free.mout = nil end
    S.free.mout = midi.connect(real_port)
    if S.free.mout then print("FREE MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
    else print("FREE MIDI OUT: connect failed for port "..real_port) end
  end

  local function free_note_on_mx(canon, note, vel) MX.on_free(canon, note, free_scaled_vel(vel)) end
  local function free_note_off_mx(canon, note) MX.off(canon, note) end
  local function free_note_on_midi(note, vel) if S.free.mout then pcall(function() S.free.mout:note_on(util.clamp(note,0,127), util.clamp(vel or 100,1,127), S.free.midi_channel) end) end end
  local function free_note_off_midi(note) if S.free.mout then pcall(function() S.free.mout:note_off(util.clamp(note,0,127), 0, S.free.midi_channel) end) end end

  local function free_fanout_on(canon, note, vel)
    if free_want_mx()   then free_note_on_mx(canon, note, vel) end
    if free_want_midi() then free_note_on_midi(note, vel) end
  end
  local function free_fanout_off(canon, note)
    if free_want_mx()   then free_note_off_mx(canon, note) end
    if free_want_midi() then free_note_off_midi(note) end
  end

  -- quantize note to current scale
  local function quantize_to_scale(note)
    local sc = scale128()
    local best_i, best_d, best_val = 1, 1e9, sc[1]
    for i=1,#sc do
      local v = sc[i]; local d = math.abs(v - note)
      if d < best_d or (d == best_d and v > best_val) then best_d, best_i, best_val = d, i, v end
    end
    return best_val, best_i
  end

  -- Free Play timing shape computation
  local function free_compute_step_offsets(m)
    local base = math.max(0, S.free.strum_steps or 0)
    local offs = {}
    if m <= 1 or base == 0 then for k=1,m do offs[k] = 0 end; return offs end

    local amt = (S.free.timing_amt or 50) / 100.0
    local spacing = {}; for k=1,m-1 do spacing[k] = base end

    local function apply_serpentine()
      local mid = (m+1)/2
      for k=1,m-1 do local d = math.abs((k - mid) / mid); spacing[k] = base * (1 + amt * d) end
    end
    local function apply_accel()
      for k=1,m-1 do local t = (k-1)/math.max(1,(m-2)); spacing[k] = math.max(0.1, base * (1 - amt * t)) end
    end
    local function apply_rit()
      for k=1,m-1 do local t = (k-1)/math.max(1,(m-2)); spacing[k] = base * (1 + amt * t) end
    end
    local function ease_in_quad(t)  return t*t end
    local function ease_out_quad(t) return 1 - (1-t)*(1-t) end
    local function apply_rake(ease_fn)
      for k=1,m-1 do
        local t = (k-1)/math.max(1,(m-2)); local w = ease_fn(t)
        spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5)*2))
        spacing[k] = math.max(0.1, spacing[k])
      end
    end
    local function apply_skip_alt()
      local add = (S.free.timing_skip_steps or 1) * amt
      for k=1,m-1 do if (k % 2) == 1 then spacing[k] = spacing[k] + add end end
    end

    if     S.free.timing_shape == 2 then apply_serpentine()
    elseif S.free.timing_shape == 3 then apply_accel()
    elseif S.free.timing_shape == 4 then apply_rit()
    elseif S.free.timing_shape == 5 then apply_rake(ease_in_quad)
    elseif S.free.timing_shape == 6 then apply_rake(ease_out_quad)
    elseif S.free.timing_shape == 7 then apply_skip_alt() end

    local sum = 0
    for k=1,m do if k == 1 then offs[k] = 0 else sum = sum + util.round(spacing[k-1]); offs[k] = sum end end
    return offs
  end

  local function free_make_strum_order(count)
    local tmp_state = { alt_flip=false, last_first=nil, last_last=nil }
    local ord = select(1, Strum.make(count, S.free.strum_type, tmp_state))
    return ord or {}
  end

  local function free_compute_gate_seconds()
    local sel = S.free.gate_mode or 1
    if sel == 1 then return nil end
    local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
    local bpm = clock.get_tempo()
    return (60 / bpm) * (S.tick_div or 1/4) * frac
  end

  local function free_schedule_gate_for_note(note)
    local secs = free_compute_gate_seconds()
    if not secs or secs <= 0 then return end
    local n = util.clamp(note, 0, 127)
    clock.run(function()
      clock.sleep(secs)
      if S.free.mout then pcall(function() S.free.mout:note_off(n, 0, S.free.midi_channel) end) end
      if S.mx then local fcanon = S.canonical_names[S.free.voice_index] or ""; MX.off(fcanon, n) end
    end)
  end

  local function free_active_notes_ascending()
    local t = {}
    for _,q in pairs(S.free.active_map) do t[#t+1] = q end
    table.sort(t); return t
  end
  local function free_active_names()
    local t = free_active_notes_ascending()
    if #t == 0 then return nil end
    local parts = {}
    for _,n in ipairs(t) do local nm, oc = midi_to_name_oct(n); parts[#parts+1] = string.format("%s%d", nm, oc) end
    return table.concat(parts, " ")
  end

  -- public
  return {
    setup_midi_out = setup_free_midi_out_awake,
    fanout_on  = free_fanout_on,
    fanout_off = free_fanout_off,
    make_order = free_make_strum_order,
    step_offs  = free_compute_step_offsets,
    gate_for   = free_schedule_gate_for_note,
    act_names  = free_active_names,
    want_mx    = free_want_mx,
    want_midi  = free_want_midi,
    quantize_to_scale = quantize_to_scale,
  }
end)()

-- ===== Event queue / clock =====
local function queue_in_steps(steps, fn) table.insert(S.evq, {steps=math.max(0, steps), fn=fn}) end
local function schedule(step_steps, fn) if not S.quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end
local function free_schedule(step_steps, fn) if step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

local function clock_loop()
  while true do
    local len = S.tick_div
    if S.swing_mode == 2 and S.swing_percent > 0 then
      local s = S.swing_percent / 100
      if S.swing_phase == 0 then len = S.tick_div * (1 + s) else len = S.tick_div * (1 - s) end
      S.swing_phase = 1 - S.swing_phase
    else S.swing_phase = 0 end

    clock.sync(len)

    local remain = {}
    for _,e in ipairs(S.evq) do
      e.steps = e.steps - 1
      if e.steps <= 0 then e.fn() else table.insert(remain, e) end
    end
    S.evq = remain
    redraw()
  end
end

-- ===== Panic helpers =====
local function panic_all_outputs()
  if S.mx and S.mx.all_notes_off then pcall(function() S.mx:all_notes_off() end) end
  MIDI.off_all()

  -- NEW: chord voice active-offs
  do
    local canon = S.canonical_names[S.voice_index] or ""
    for _,notes in pairs(S.chord_active) do
      for _,n in ipairs(notes) do fanout_note_off(canon, n) end
    end
    S.chord_active = {}
  end

  -- free off
  local fcanon = S.canonical_names[S.free.voice_index] or ""
  S.free.hold_tokens = {}
  for _,q in pairs(S.free.active_map) do Free.fanout_off(fcanon, q) end
  for _,outs in pairs(S.free.chord_active) do for _,n in ipairs(outs) do Free.fanout_off(fcanon, n) end end
  S.free.active_map = {}; S.free.chord_active = {}

  -- arp off
  Arp.stop()
end

-- ===== Chord voice handlers =====
local strum_state = { alt_flip=false, last_first=nil, last_last=nil }

local function make_strum_order(count)
  local ord, ns = Strum.make(count, S.strum_type, strum_state)
  strum_state = ns
  return ord
end

local function handle_note_on(canon, root_note, vel, deg)
  -- Defensive: if this root is already active, turn it off first to avoid overlaps
  if S.chord_active[root_note] then
    for _,n in ipairs(S.chord_active[root_note]) do fanout_note_off(canon, n) end
    S.chord_active[root_note] = nil
  end

  local chord, qual, name_root_midi = chord_for_degree(root_note, deg, S.chord_type, S.inversion, S.spread, S.sus_mode)
  S.last_voiced_notes = {}; for i,n in ipairs(chord) do S.last_voiced_notes[i] = n end
  S.last_bass_note = chord[1]

  -- NEW: record emitted notes for this root key
  S.chord_active[root_note] = { table.unpack(chord) }

  S.last_chord_name = build_chord_display_name(chord, qual, name_root_midi); S.last_name_time = now()

  -- notify Arp (refresh + key down; restarts phase per spec)
  Arp.refresh()
  Arp.chord_key(true)

  local order = make_strum_order(#chord)
  local offs  = compute_step_offsets(#order)

  local sorted = clone_sorted(chord)
  local idx_in_sorted = {}; for i,n in ipairs(sorted) do if idx_in_sorted[n]==nil then idx_in_sorted[n]=i end end
  local n_sorted = #sorted

  local flam_on    = (params:get("chorder_flam") or 1) == 2
  local flam_cnt   = params:get("chorder_flam_count") or 0
  local flam_space = params:get("chorder_flam_space") or 1
  local flam_dvel  = params:get("chorder_flam_vel") or -8

  local hold_on = (params:get("chorder_hold_strum") or 1) == 2
  local tid = next_trigger_id()
  if hold_on then S.chord_hold_tokens[root_note] = tid end

  for k,idx in ipairs(order) do
    local n = chord[idx]
    local s = (offs[k-1] or 0) + jitter_steps(S.humanize_steps_max)
    local v_base = jitter_velocity(vel, S.humanize_vel_range)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = apply_velocity_profile(k, #order, v_base, pos_in_sorted, n_sorted)

    schedule(s, function()
      if (not hold_on) or (S.chord_hold_tokens[root_note] == tid) then
        fanout_note_on(canon, n, v)
      end
    end)

    if flam_on and flam_cnt > 0 then
      for j=1, flam_cnt do
        local ds = j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        schedule(s + ds, function()
          if (not hold_on) or (S.chord_hold_tokens[root_note] == tid) then
            fanout_note_on(canon, n, vv)
          end
        end)
      end
    end
  end
end

local function handle_note_off(canon, root_note)
  -- notify Arp (key up)
  Arp.chord_key(false)

  if (params:get("chorder_hold_strum") or 1) == 2 then
    S.chord_hold_tokens[root_note] = nil
  end

  -- NEW: turn off exactly what we turned on
  local notes = S.chord_active[root_note]
  if notes then
    for _,n in ipairs(notes) do fanout_note_off(canon, n) end
    S.chord_active[root_note] = nil
  end
end

-- ===== Free handlers =====
local function free_handle_note_on(in_note, in_vel)
  local play_vel = (S.free.velocity_mode == 1) and S.free.fixed_velocity or util.clamp(in_vel or 100, 1, 127)
  local fcanon = S.canonical_names[S.free.voice_index] or ""

  -- ===== CHROMATIC (mono only, no quantize) =====
  if S.free.key_mode == 3 then
    local base = util.clamp(in_note + 12 * (S.free.transpose_oct or 0), 0, 127)
    S.free.active_map[in_note] = base
    Free.fanout_on(fcanon, base, play_vel)
    Free.gate_for(base)
    return
  end

  -- ===== DIATONIC / QUANTIZED MODES =====
  local sc = scale128()
  local deg, base

  if S.free.key_mode == 1 then
    -- White keys → diatonic degrees (ignore black keys)
    local pc = in_note % 12
    local d = K.WHITE_TO_DEG[pc]; if not d then return end
    deg = d
    local i_deg = nearest_index_with_degree(sc, in_note, deg)
    base = sc[util.clamp(i_deg, 1, #sc)]
  else
    -- key_mode == 2 → all keys quantized to nearest scale tone
    local qn, qi = Free.quantize_to_scale(in_note)
    deg = 1 + ((qi - 1) % 7)
    base = qn
  end

  base = util.clamp(base + 12 * (S.free.transpose_oct or 0), 0, 127)

  if S.free.mode == 1 then
    -- MONO
    S.free.active_map[in_note] = base
    Free.fanout_on(fcanon, base, play_vel)
    Free.gate_for(base)
    return
  end

  -- CHORD (uses free recipe, still independent of chord voice)
  local save_voicing, save_add = S.voicing, S.add_bass
  S.voicing, S.add_bass = S.free.voicing, (S.free.add_bass == 2)
  local free_chord_type = (S.free.seventh == 2) and 2 or 1
  local free_chord_notes, qual, name_root_midi = chord_for_degree(base, deg, free_chord_type, S.free.inversion, S.free.spread, 1)
  S.voicing, S.add_bass = save_voicing, save_add

  table.sort(free_chord_notes)
  local ord = (#free_chord_notes > 1 and S.free.strum_steps > 0) and Free.make_order(#free_chord_notes)
              or (function(n) local t = {}; for i = 1, n do t[i] = i end; return t end)(#free_chord_notes)
  local offs = Free.step_offs(#ord)

  local hold_on = (params:get("free_hold_strum") or 1) == 2
  local tid = next_trigger_id()
  if hold_on then S.free.hold_tokens[in_note] = tid end

  local emitted = {}
  for k, idx in ipairs(ord) do
    local n = free_chord_notes[idx]
    local s = offs[k] or 0
    free_schedule(s, function()
      if (not hold_on) or (S.free.hold_tokens[in_note] == tid) then
        Free.fanout_on(fcanon, n, play_vel)
        Free.gate_for(n)
      end
    end)
    emitted[#emitted + 1] = n
  end

  S.free.chord_active[in_note] = emitted

  -- HUD label for Free chord
  do
    local root_pc_val = name_root_midi % 12
    local symbol = base_chord_symbol(root_pc_val, qual, free_chord_type, 1)
    local bass = free_chord_notes[1]; for i = 2, #free_chord_notes do if free_chord_notes[i] < bass then bass = free_chord_notes[i] end end
    local bass_pc = bass % 12
    if bass_pc ~= root_pc_val then
      symbol = symbol .. "/" .. K.NOTE_NAMES_SHARP[bass_pc + 1]
    end
    local tags = {}
    if S.free.inversion > 0 and (S.free.voicing ~= 11) then table.insert(tags, "inv"..S.free.inversion) end
    if     S.free.voicing == 2 then table.insert(tags, "drop2")
    elseif S.free.voicing == 3 then table.insert(tags, "drop3")
    elseif S.free.voicing == 4 then table.insert(tags, "drop2&4")
    elseif S.free.voicing == 5 then table.insert(tags, "drop1")
    elseif S.free.voicing == 6 then table.insert(tags, "open")
    elseif S.free.voicing == 7 then table.insert(tags, "wide")
    elseif S.free.voicing == 8 then table.insert(tags, "quartal")
    elseif S.free.voicing == 9 then table.insert(tags, "quintal")
    elseif S.free.voicing == 10 then table.insert(tags, "nearest")
    elseif S.free.voicing == 11 then table.insert(tags, "smooth") end
    if (S.free.add_bass == 2) and bass_pc == root_pc_val then table.insert(tags, "bass") end
    if S.free.spread ~= 0 then local sgn = (S.free.spread > 0) and ("+"..S.free.spread) or tostring(S.free.spread); table.insert(tags, "spread"..sgn) end
    if #tags > 0 then symbol = symbol .. " (" .. table.concat(tags, ", ") .. ")" end
    S.free.last_name = symbol
    S.free.last_time = util.time()
  end
end

local function free_handle_note_off(in_note)
  if S.free.mode == 1 or S.free.key_mode == 3 then
    local q = S.free.active_map[in_note]
    if q ~= nil then
      S.free.active_map[in_note] = nil
      local fcanon = S.canonical_names[S.free.voice_index] or ""
      Free.fanout_off(fcanon, q)
    end
    return
  end
  if (params:get("free_hold_strum") or 1) == 2 then
    S.free.hold_tokens[in_note] = nil
  end
  local outs = S.free.chord_active[in_note]
  if outs then
    local fcanon = S.canonical_names[S.free.voice_index] or ""
    for _,n in ipairs(outs) do Free.fanout_off(fcanon, n) end
    S.free.chord_active[in_note] = nil
  end
end

-- ===== UI =====
local function draw_header(active_label)
  screen.level(15); screen.move(4, 12); screen.text("CHORDER")
  screen.level(10); screen.move(124, 12); if active_label then screen.text_right("["..active_label.."]") end
end
local function draw_line(y, label, value)
  screen.level(12); screen.move(4, y); screen.text(label or "")
  screen.level(15); screen.move(124, y); screen.text_right(value or "")
end
local function key_center_string()
  local name = K.NOTE_NAMES_SHARP[S.root_pc+1]; local rname, roct = midi_to_name_oct(S.root_midi)
  return string.format("%s (root %s%d)", name, rname, roct)
end
local function free_key_mode_label() if     S.free.key_mode == 1 then return "white" elseif S.free.key_mode == 2 then return "quantized" else return "chromatic" end end

local function draw_output_page()
  draw_header("I/O")

  local out_mode_full  = OUT_OPTS[params:get(S.out_mode_param) or 1] or "?"
  local out_mode_short = short_mode_name(out_mode_full)

  local out_idx  = params:get(S.midi.out_dev_param) or 1
  local out_lbl  = ellipsize(S.midi.devices[out_idx] or "—", 12)
  local mo_ch    = tostring(params:get(S.midi.out_ch_param) or 1)

  local mi_i      = params:get(S.midi.in_dev_param) or 1
  local mi_lbl    = ellipsize(S.midi.in_devices[mi_i] or "none", 12)
  local ch_in_idx = params:get(S.midi.in_ch_param) or 1
  local ch_in_disp = (ch_in_idx==1) and "Omni" or ("Ch"..tostring(ch_in_idx-1))

  local free_on        = (params:get("free_enable")==2)
  local free_mode_full = S.free.out_opts[params:get(S.free.out_mode_param) or 1] or "?"
  local free_mode_short = short_mode_name(free_mode_full)
  local free_dev_i     = params:get(S.free.midi_out_dev_param) or 1
  local free_dev       = ellipsize(S.midi.devices[free_dev_i] or "—", 12)
  local free_ch        = tostring(params:get(S.free.midi_out_ch_param) or 1)
  local free_in_ch     = tostring(params:get(S.free.midi_in_ch_param) or 2)

  screen.level(12); screen.move(4, 22); screen.text("Out:")
  screen.level(15); screen.move(124,22)
  if out_mode_short == "MIDI" or out_mode_short == "mx+M" then
    screen.text_right(out_mode_short.." | "..out_lbl.." / Ch"..mo_ch)
  else
    screen.text_right(out_mode_short)
  end

  screen.level(12); screen.move(4, 34); screen.text("In:")
  screen.level(15); screen.move(124,34); screen.text_right(mi_lbl.." / "..ch_in_disp)

  screen.level(12); screen.move(4, 46); screen.text("Free:")
  screen.level(15); screen.move(124,46)
  if free_on then screen.text_right("on | "..free_mode_short.." | InCh "..free_in_ch)
  else screen.text_right("off") end

  if want_mx() then
    local cur_name = S.display_names[S.voice_index] or "(no packs)"
    screen.level(12); screen.move(4, 58); screen.text("mx.samples:")
    screen.level(15); screen.move(124,58); screen.text_right(ellipsize(cur_name, 18))
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
  draw_line(40, "Scale:", S.scale_name)

  local fp = Free.act_names()
  screen.level(fp and 12 or 10); screen.move(64, 50)
  screen.text_center("Free: " .. (fp or "—"))

  local now_t = now()
  local show_name = S.last_chord_name and ((now_t - S.last_name_time) < S.last_name_timeout)
  screen.level(15)
  if show_name then
    screen.move(64, 60); screen.text_center(ellipsize(S.last_chord_name, 26))
  else
    screen.level(10); screen.move(64, 60); screen.text_center("(play a chord)")
  end
end

local function draw_hud_page()
  screen.clear()
  screen.level(9); screen.move(64, 10)
  local arp_on = (params:get("arp_enable")==2)
  local trig = (params:get("arp_trigger_mode")==1) and "key-held" or "latch"
  screen.text_center("Arp: "..(arp_on and trig or "off"))
  screen.level(9); screen.move(64, 14); screen.text_center("(free: "..free_key_mode_label()..")")

  local now_t = now()
  local show_chord = S.last_chord_name and ((now_t - S.last_name_time) < S.last_name_timeout)
  local show_free  = S.free.last_name and ((now_t - S.free.last_time) < S.last_name_timeout)

  if show_chord then
    screen.level(15); pcall(function() screen.font_size(12) end)
    screen.move(64, 28); screen.text_center(ellipsize(S.last_chord_name, 26))
    pcall(function() screen.font_size(8) end)
  else
    screen.level(10); screen.move(64, 28); screen.text_center("(play a chord)")
  end

  if show_free then
    screen.level(12); pcall(function() screen.font_size(10) end)
    screen.move(64, 44); screen.text_center(ellipsize(S.free.last_name, 26))
    pcall(function() screen.font_size(8) end)
  else
    screen.level(10); screen.move(64, 44); screen.text_center("(free chord)")
  end

  local fp = Free.act_names()
  screen.level(fp and 12 or 10); screen.move(64, 58)
  screen.text_center("Free: " .. (fp or "—"))
  screen.update()
end

function redraw()
  screen.clear()
  if S.page == S.PAGE_HUD then draw_hud_page(); return end
  if S.page == S.PAGE_OUTPUT then draw_output_page() else draw_chord_page() end
  screen.update()
end

-- ===== lifecycle =====
local function show_param(id, show) if show then params:show(id) else params:hide(id) end end
local _sep_counter = 0
local function add_section(title, builders) params:add_group(title, #builders); for _,fn in ipairs(builders) do fn() end end
local function div(label) return function() _sep_counter = _sep_counter + 1; params:add_separator(("— %s — [%d]"):format(label or "", _sep_counter)) end end

local function update_free_visibility()
  local chroma = (S.free.key_mode == 3)
  show_param("free_mode", not chroma)
  show_param("free_seventh", not chroma)
  show_param("free_inversion", not chroma)
  show_param("free_spread", not chroma)
  show_param("free_voicing", not chroma)
  show_param("free_add_bass", not chroma)
  show_param("free_strum_steps", not chroma)
  show_param("free_strum_type", not chroma)
  show_param("free_timing_shape", not chroma)
  show_param("free_timing_amt", not chroma)
  show_param("free_timing_skip_steps", not chroma)
end

-- ===== MIDI In setup (demux) =====
local function teardown_midi_in()
  if S.midi.input then S.midi.input.event = nil; S.midi.input = nil end
end

local function setup_midi_in(param_index)
  teardown_midi_in()
  local port = S.midi.in_ports_map[param_index] or 0
  if port == 0 then
    print("MIDI IN: no input selected")
    return
  end

  S.midi.input = midi.connect(port)
  if not S.midi.input then
    print("MIDI IN: connect failed for port "..port)
    return
  end

  S.midi.input.event = function(data)
    local msg = midi.to_msg(data); if not msg then return end

    -- chord input channel filter (supports omni)
    local chord_ok
    do
      local ch_sel_idx = params:get(S.midi.in_ch_param)
      chord_ok = (ch_sel_idx == 1) or (msg.ch == (ch_sel_idx - 1))
    end

    -- free play input channel (explicit 1..16; default 2)
    local free_ok
    do
      local free_ch = params:get(S.free.midi_in_ch_param) or 2
      free_ok = (msg.ch == free_ch)
    end

    local canon = S.canonical_names[S.voice_index] or ""
    local chord_vel = (S.velocity_mode == 2 and (msg.vel or S.fixed_velocity) or S.fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      -- CHORD voice (white keys = diatonic degrees)
      if chord_ok then
        MX.ensure_loaded(S.voice_index)
        local pc = msg.note % 12
        if K.WHITE_SET[pc] then
          local deg = K.WHITE_TO_DEG[pc]
          if deg then handle_note_on(canon, msg.note, chord_vel, deg) end
        end
      end

      -- FREE PLAY (mode depends on free_key_mode)
      if S.free.enable and free_ok then
        local vel = (S.free.velocity_mode == 1) and S.free.fixed_velocity or msg.vel
        free_handle_note_on(msg.note, vel)
      end

      redraw()

    elseif (msg.type == "note_off") or (msg.type == "note_on" and msg.vel == 0) then
      if chord_ok then
        local pc = msg.note % 12
        if K.WHITE_SET[pc] then handle_note_off(canon, msg.note) end
      end
      if S.free.enable and free_ok then free_handle_note_off(msg.note) end
      redraw()
    end
  end

  print("MIDI IN: connected to "..(S.midi.in_devices[param_index] or ("port "..port)))
end

local function rebind_midi_in_if_needed()
  if (S.midi.input == nil) or (S.midi.input.event == nil) then
    local cur = params:get(S.midi.in_dev_param) or 1
    setup_midi_in(cur)
  end
end

-- Helper to build Arp device options with sentinel
local function arp_device_options()
  local t = {"(use Chord MIDI out)"} -- sentinel index 1
  for i=1,#S.midi.devices do t[#t+1] = S.midi.devices[i] end
  return t
end

local function rebuild_midi_lists()
  local remembered_in_long = nil
  do
    local cur_idx = params:get(S.midi.in_dev_param) or 1
    local vp = S.midi.in_ports_map[cur_idx]
    if vp and midi.vports[vp] then remembered_in_long = midi.vports[vp].name end
  end

  MIDI.build_out()
  MIDI.build_in()

  params:hide(S.midi.in_dev_param);  params:show(S.midi.in_dev_param)
  local p_in = params:lookup_param(S.midi.in_dev_param)
  if p_in then p_in.options = S.midi.in_devices; p_in.count = #S.midi.in_devices end

  params:hide(S.midi.out_dev_param); params:show(S.midi.out_dev_param)
  local p_out = params:lookup_param(S.midi.out_dev_param)
  if p_out then p_out.options = S.midi.devices; p_out.count = #S.midi.devices end

  params:hide(S.free.midi_out_dev_param); params:show(S.free.midi_out_dev_param)
  local p_free_out = params:lookup_param(S.free.midi_out_dev_param)
  if p_free_out then p_free_out.options = S.midi.devices; p_free_out.count = #S.midi.devices end

  -- also refresh Arp device list combo (with sentinel)
  params:hide("arp_midi_out_dev"); params:show("arp_midi_out_dev")
  local p_arp_out = params:lookup_param("arp_midi_out_dev")
  if p_arp_out then
    p_arp_out.options = arp_device_options()
    p_arp_out.count   = #p_arp_out.options
  end

  if remembered_in_long then
    local idx = 1
    for i = 2, #S.midi.in_devices do
      local vp = S.midi.in_ports_map[i]
      if vp and midi.vports[vp] and midi.vports[vp].name == remembered_in_long then idx = i; break end
    end
    params:set(S.midi.in_dev_param, idx)
    setup_midi_in(idx)
  end
end

-- ===== init =====
local function add_arp_section()
  add_section("CHORDER · Arpeggio", {
    div("Toggle & Trigger"),
    function()
      params:add_option("arp_enable", "arpeggio", {"off","on"}, 1)
      params:set_action("arp_enable", function(i) if i==2 then Arp.start() else Arp.stop() end end)
    end,
    function() params:add_option("arp_trigger_mode", "trigger mode", {"key-held","latch"}, 1) end,
    function() params:add_option("arp_retrack", "retrack at cycle", {"off","on"}, 1) end,

    div("Material"),
    function() params:add_option("arp_material", "material", {"chord tones","chord + passing"}, 2) end,

    div("Order & Range"),
    function() params:add_option("arp_order", "order", K.STRUM_OPTS, 1) end,
    function() params:add_number("arp_octaves", "octave span", 0, 3, 1) end,
    function() params:add_option("arp_oct_walk", "octave walk", {"wrap","bounce"}, 1) end,

    div("Timing"),
    function() params:add_option("arp_div", "division", K.QUANT_DIV_OPTS, 6) end, -- 1/8
    function() params:add_option("arp_swing_mode", "swing mode", {"grid","swing %"}, 1) end,
    function() params:add_number("arp_swing_pct", "swing %", 0, 75, 0) end,

    div("Velocity & Gate"),
    function() params:add_number("arp_vel", "base velocity", 1, 127, 100) end,
    function() params:add_number("arp_vel_ramp", "vel ramp/step", -24, 24, 0) end,
    function() params:add_option("arp_gate", "gate", {"release","25%","50%","75%","100%"}, 3) end,

    div("Output"),
    function()
      params:add_option("arp_out_mode", "output", {"mx.samples","mx.samples + MIDI","MIDI"}, 1)
      -- When output mode changes, refresh arp routing so fallback logic is accurate right away
      params:set_action("arp_out_mode", function(_) Arp.hotplug_refresh() end)
    end,
    function()
      params:add_option("arp_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action("arp_mx_voice", function(i) MX.ensure_loaded(i) end)
    end,
    function()
      params:add_option("arp_midi_out_dev", "MIDI out", (function()
        local t = {"(use Chord MIDI out)"} -- sentinel at index 1
        for i=1,#S.midi.devices do t[#t+1] = S.midi.devices[i] end
        return t
      end)(), 1)
      params:set_action("arp_midi_out_dev", function(i)
        if i == 1 then
          -- sentinel: use Chord MIDI out
          print("ARP MIDI OUT: using Chord MIDI out")
          -- keep A.mout=nil; fanout uses fallback S.midi.out
        else
          Arp.setup_midi_out(i - 1) -- shift for sentinel
        end
      end)
    end,
    function() params:add_option("arp_midi_out_ch", "MIDI out ch", (function() local t={} for i=1,16 do t[i]=tostring(i) end; return t end)(), 1) end,
  })
end

local function update_arp_visibility()
  show_param("arp_midi_out_dev", true)
  show_param("arp_midi_out_ch",  true)
end

function init()
  S.mx = mxsamples:new()
  MX.scan()
  MIDI.build_out()
  MIDI.build_in()

  local default_midi_in_index = (function(long_name)
    if not long_name or long_name == "" then return 1 end
    for i = 2, #S.midi.in_devices do
      local vp = S.midi.in_ports_map[i]
      local ln = midi.vports[vp] and midi.vports[vp].name
      if ln == long_name then return i end
    end
    return 1
  end)(DEFAULT_MIDI_IN_NAME)

  -- === SECTION 1: CHORDER · I/O ===
  local g_io = {
    div("Output"),
    function()
      params:add_option(S.out_mode_param, "output", OUT_OPTS, 1)
      params:set_action(S.out_mode_param, function(_)
        local includes_midi = want_midi()
        show_param(S.midi.gate_param, includes_midi)
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,

    div("MIDI Out"),
    function()
      params:add_option(S.midi.out_dev_param, "MIDI output", S.midi.devices, 1)
      params:set_action(S.midi.out_dev_param, function(i)
        panic_all_outputs()
        MIDI.setup_out(i)
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,
    function()
      params:add_option(S.midi.out_ch_param, "MIDI output channel", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 1)
      params:set_action(S.midi.out_ch_param, function(idx)
        S.midi.channel = idx
        panic_all_outputs()
        rebind_midi_in_if_needed()
        redraw()
      end)
    end,

    div("MIDI In"),
    function()
      params:add_option(S.midi.in_dev_param, "MIDI input", S.midi.in_devices, default_midi_in_index)
      params:set_action(S.midi.in_dev_param, function(i) setup_midi_in(i) end)
    end,
    function()
      local ch_opts_in = {"omni"}; for i=1,16 do ch_opts_in[#ch_opts_in+1]=tostring(i) end
      params:add_option(S.midi.in_ch_param, "chord MIDI input ch", ch_opts_in, 1)
    end,

    div("Instruments (Chord)"),
    function()
      params:add_option("chorder_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action("chorder_mx_voice", function(i) S.voice_index = i; MX.ensure_loaded(i); redraw() end)
    end,
    function()
      params:add_trigger("chorder_refresh_voices", "refresh instrument list")
      params:set_action("chorder_refresh_voices", function()
        MX.scan()
        local p = params:lookup_param("chorder_mx_voice")
        if p then
          p.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
          p.count   = (#S.display_names>0 and #S.display_names or 1)
          S.voice_index = 1; params:set("chorder_mx_voice", S.voice_index)
        end
        local pf = params:lookup_param("free_mx_voice")
        if pf then
          pf.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
          pf.count   = (#S.display_names>0 and #S.display_names or 1)
          S.free.voice_index = 1; params:set("free_mx_voice", S.free.voice_index)
        end
        -- also refresh Arp voice options
        local pa = params:lookup_param("arp_mx_voice")
        if pa then
          pa.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
          pa.count   = (#S.display_names>0 and #S.display_names or 1)
        end
        redraw()
      end)
    end,

    div("Levels"),
    function()
      params:add_number("chorder_mx_vol_pct", "mx volume (%)", 0, 200, S.mx_vol_pct)
      params:set_action("chorder_mx_vol_pct", function(v) S.mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,
  }
  add_section("CHORDER · I/O", g_io)

  -- === SECTION 2: CHORDER · Musical Setup ===
  local VOICE_OPTS = {
    "none","drop-2","drop-3","drop-2&4","drop-1",
    "open","wide","quartal","quintal","nearest","smooth"
  }

  local g_setup = {
    div("Key & Scale"),
    function()
      params:add_option("chorder_root_pc", "root pitch", K.NOTE_NAMES_SHARP, S.root_pc+1)
      params:set_action("chorder_root_pc", function(i) S.root_pc = i-1; recompute_root_midi(); redraw() end)
    end,
    function()
      params:add_number("chorder_root_oct", "root octave", -1, 8, S.root_oct)
      params:set_action("chorder_root_oct", function(v) S.root_oct = util.clamp(v,-1,8); recompute_root_midi(); redraw() end)
    end,
    function()
      params:add_option("chorder_scale", "scale/mode", (function()
        local names = {}
        for i = 1, #musicutil.SCALES do names[i] = musicutil.SCALES[i].name end
        if #names > 0 then S.scale_name = names[1] end
        return names
      end)(), 1)
      params:set_action("chorder_scale", function(i) S.scale_name = (musicutil.SCALES[i] and musicutil.SCALES[i].name) or "Major"; redraw() end)
    end,

    div("Chord Build"),
    function()
      params:add_option("chorder_chord_type", "chord type", {"triad","7th","9th"}, S.chord_type)
      params:set_action("chorder_chord_type", function(i) S.chord_type = i; redraw() end)
    end,
    function()
      params:add_option("chorder_sus_mode", "third handling", {"normal","sus2","sus4"}, S.sus_mode)
      params:set_action("chorder_sus_mode", function(i) S.sus_mode = i; redraw() end)
    end,
    function()
      params:add_number("chorder_inversion", "inversion (0-3)", 0, 3, S.inversion)
      params:set_action("chorder_inversion", function(v) S.inversion = util.clamp(v,0,3); redraw() end)
    end,
    function()
      params:add_number("chorder_spread", "spread (semitones)", -24, 24, S.spread)
      params:set_action("chorder_spread", function(v) S.spread = util.round(v); redraw() end)
    end,

    div("Voicing & Bass"),
    function()
      params:add_option("chorder_voicing", "voicing", VOICE_OPTS, S.voicing)
      params:set_action("chorder_voicing", function(i) S.voicing = i; show_param("chorder_inversion", S.voicing ~= 11); redraw() end)
    end,
    function()
      params:add_option("chorder_bass_note", "add bass (root -12)", {"off","on"}, (S.add_bass and 2 or 1))
      params:set_action("chorder_bass_note", function(i) S.add_bass = (i==2); redraw() end)
    end,

    div("Velocity (Chord)"),
    function()
      params:add_option("chorder_vel_mode", "velocity src (chord)", {"fixed","incoming"}, S.velocity_mode)
      params:set_action("chorder_vel_mode", function(i) S.velocity_mode = i; show_param("chorder_vel_fixed", i==1) end)
    end,
    function()
      params:add_number("chorder_vel_fixed", "fixed velocity (chord)", 1, 127, S.fixed_velocity)
      params:set_action("chorder_vel_fixed", function(v) S.fixed_velocity = util.clamp(v,1,127) end)
    end,
  }
  add_section("CHORDER · Musical Setup", g_setup)

  -- === SECTION 3: CHORDER · Timing & Feel ===
  local g_timing = {
    div("Clock & Grid"),
    function()
      params:add_option("chorder_quantize", "quantize chords", {"off","on"}, (S.quantize and 2 or 1))
      params:set_action("chorder_quantize", function(i) S.quantize = (i==2); redraw() end)
    end,
    function()
      params:add_option("chorder_quant_div", "quantize division", K.QUANT_DIV_OPTS, 4)
      params:set_action("chorder_quant_div", function(i) S.tick_div_str = K.QUANT_DIV_OPTS[i]; S.tick_div = K.QUANT_DIV_MAP[S.tick_div_str] or 1/4; redraw() end)
    end,
    function()
      params:add_option("chorder_swing_mode", "swing mode", {"grid","swing %"}, S.swing_mode)
      params:set_action("chorder_swing_mode", function(i) S.swing_mode = i; show_param("chorder_swing_pct", S.swing_mode == 2); redraw() end)
    end,
    function()
      params:add_number("chorder_swing_pct", "swing %", 0, 75, S.swing_percent)
      params:set_action("chorder_swing_pct", function(v) S.swing_percent = util.clamp(v, 0, 75); redraw() end)
    end,

    div("Gate (Chord MIDI)"),
    function() params:add_option(S.midi.gate_param, "MIDI gate", S.midi.gate_opts, 1) end,

    div("Strum"),
    function()
      params:add_number("chorder_strum", "strum (steps of division)", 0, 8, S.strum_steps)
      params:set_action("chorder_strum", function(v) S.strum_steps = util.clamp(v,0,8); redraw() end)
    end,
    function()
      params:add_option("chorder_strum_type", "strum type", K.STRUM_OPTS, S.strum_type)
      params:set_action("chorder_strum_type", function(i) S.strum_type = i; strum_state = { alt_flip=false, last_first=nil, last_last=nil }; redraw() end)
    end,
    function() params:add_option("chorder_hold_strum", "hold-to-strum (Chord)", {"off","on"}, 1) end,

    div("Timing Shapes"),
    function()
      params:add_option("chorder_timing_shape", "timing shape",
        {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, 1)
    end,
    function() params:add_number("chorder_timing_amt", "timing amount", 0, 100, 50) end,
    function() params:add_number("chorder_timing_skip_steps", "skip size (steps)", 0, 8, 1) end,

    div("Humanize"),
    function()
      params:add_number("chorder_hum_steps", "humanize timing (max steps)", 0, 4, S.humanize_steps_max)
      params:set_action("chorder_hum_steps", function(v) S.humanize_steps_max = util.clamp(v,0,4); redraw() end)
    end,
    function()
      params:add_number("chorder_hum_vel", "humanize velocity (+/-)", 0, 30, S.humanize_vel_range)
      params:set_action("chorder_hum_vel", function(v) S.humanize_vel_range = util.clamp(v,0,30); redraw() end)
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

  -- === SECTION 4: CHORDER · Free Play ===
  local g_free = {
    div("Toggle"),
    function()
      params:add_option("free_enable", "free play", {"off","on"}, 1)
      params:set_action("free_enable", function(i) S.free.enable = (i==2); if not S.free.enable then panic_all_outputs() end; redraw() end)
    end,

    div("Instrument & Output"),
    function()
      params:add_option("free_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action("free_mx_voice", function(i) S.free.voice_index = i; MX.ensure_loaded(i); redraw() end)
    end,
    function()
      params:add_option(S.free.out_mode_param, "output", S.free.out_opts, 1)
      params:set_action(S.free.out_mode_param, function(_) panic_all_outputs(); rebind_midi_in_if_needed(); show_param("free_gate_mode", Free.want_midi()); redraw() end)
    end,

    div("MIDI Out"),
    function()
      params:add_option(S.free.midi_out_dev_param, "MIDI out", S.midi.devices, 1)
      params:set_action(S.free.midi_out_dev_param, function(i) Free.setup_midi_out(i); panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,
    function()
      params:add_option(S.free.midi_out_ch_param, "MIDI out ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 1)
      params:set_action(S.free.midi_out_ch_param, function(idx) S.free.midi_channel = idx; panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,

    div("MIDI In"),
    function() params:add_option(S.free.midi_in_ch_param, "MIDI input ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 2) end,

    div("Key Input Mode"),
    function()
      params:add_option(S.free.key_mode_param, "free key mode",
        {"white keys (diatonic degrees)","all keys → quantized to scale","all keys chromatic (mono only)"}, S.free.key_mode)
      params:set_action(S.free.key_mode_param, function(i)
        S.free.key_mode = i
        if S.free.key_mode == 3 then
          params:set("free_mode", 1); S.free.mode = 1
          panic_all_outputs()
        end
        update_free_visibility()
        redraw()
      end)
    end,

    div("Mode"),
    function()
      params:add_option("free_mode", "play mode", {"mono","chord"}, S.free.mode)
      params:set_action("free_mode", function(i) S.free.mode = i end)
    end,

    div("Chord Recipe"),
    function()
      params:add_option("free_seventh", "type", {"triad","7th"}, S.free.seventh)
      params:set_action("free_seventh", function(i) S.free.seventh = i end)
    end,
    function()
      params:add_number("free_inversion", "inversion (0-3)", 0, 3, S.free.inversion)
      params:set_action("free_inversion", function(v) S.free.inversion = util.clamp(v,0,3) end)
    end,
    function()
      params:add_number("free_spread", "spread (semitones)", -24, 24, S.free.spread)
      params:set_action("free_spread", function(v) S.free.spread = util.round(v) end)
    end,
    function()
      params:add_option("free_voicing", "voicing",
        {"none","drop-2","drop-3","drop-2&4","drop-1","open","wide","quartal","quintal","nearest","smooth"}, S.free.voicing)
      params:set_action("free_voicing", function(i) S.free.voicing = i end)
    end,
    function()
      params:add_option("free_add_bass", "add bass (root -12)", {"off","on"}, S.free.add_bass)
      params:set_action("free_add_bass", function(i) S.free.add_bass = i end)
    end,

    div("Strum (Free)"),
    function()
      params:add_number("free_strum_steps", "strum (steps of division)", 0, 8, S.free.strum_steps)
      params:set_action("free_strum_steps", function(v) S.free.strum_steps = util.clamp(v,0,8) end)
    end,
    function()
      params:add_option("free_strum_type", "strum type", K.STRUM_OPTS, S.free.strum_type)
      params:set_action("free_strum_type", function(i) S.free.strum_type = i end)
    end,
    function() params:add_option("free_hold_strum", "hold-to-strum (Free)", {"off","on"}, 1) end,

    div("Timing (Free)"),
    function()
      params:add_option("free_timing_shape", "timing shape",
        {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, S.free.timing_shape)
      params:set_action("free_timing_shape", function(i) S.free.timing_shape = i end)
    end,
    function()
      params:add_number("free_timing_amt", "timing amount", 0, 100, S.free.timing_amt)
      params:set_action("free_timing_amt", function(v) S.free.timing_amt = util.clamp(v,0,100) end)
    end,
    function()
      params:add_number("free_timing_skip_steps", "skip size (steps)", 0, 8, S.free.timing_skip_steps)
      params:set_action("free_timing_skip_steps", function(v) S.free.timing_skip_steps = util.clamp(v,0,8) end)
    end,

    div("Velocity & Gate (Free)"),
    function()
      params:add_option("free_vel_mode", "velocity src", {"fixed","incoming"}, S.free.velocity_mode)
      params:set_action("free_vel_mode", function(i) S.free.velocity_mode = i; show_param("free_vel_fixed", i==1) end)
    end,
    function()
      params:add_number("free_vel_fixed", "fixed velocity", 1, 127, S.free.fixed_velocity)
      params:set_action("free_vel_fixed", function(v) S.free.fixed_velocity = util.clamp(v,1,127) end)
    end,
    function()
      params:add_option("free_gate_mode", "MIDI gate", {"release","25%","50%","75%","100%"}, S.free.gate_mode)
      params:set_action("free_gate_mode", function(i) S.free.gate_mode = i end)
    end,

    div("Levels"),
    function()
      params:add_number("free_mx_vol_pct", "free mx volume (%)", 0, 200, S.free.mx_vol_pct)
      params:set_action("free_mx_vol_pct", function(v) S.free.mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,

    div("Transpose"),
    function()
      params:add_number("free_transpose_oct", "free play transpose (oct)", -4, 4, S.free.transpose_oct)
      params:set_action("free_transpose_oct", function(v) panic_all_outputs(); S.free.transpose_oct = util.clamp(v, -4, 4) end)
    end,
  }
  add_section("CHORDER · Free Play", g_free)

  -- === SECTION 5: CHORDER · Arpeggio ===
  add_arp_section()

  -- Hotplug
  midi.add = function(dev)
    print("MIDI added: "..(dev.name or "?"))
    rebuild_midi_lists()
    Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
    -- Respect sentinel for Arp:
    Arp.hotplug_refresh()
  end
  midi.remove = function(dev)
    print("MIDI removed: "..(dev.name or "?"))
    rebuild_midi_lists()
    setup_midi_in(params:get(S.midi.in_dev_param) or default_midi_in_index)
    MIDI.setup_out(params:get(S.midi.out_dev_param) or 1)
    Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
    -- Respect sentinel for Arp:
    Arp.hotplug_refresh()
  end

  -- initial visibility
  show_param("chorder_swing_pct", S.swing_mode == 2)
  show_param("chorder_vel_fixed", S.velocity_mode == 1)
  show_param("chorder_inversion", S.voicing ~= 11)
  show_param(S.midi.gate_param, want_midi())
  show_param("free_vel_fixed", S.free.velocity_mode == 1)
  show_param("free_gate_mode", Free.want_midi())
  update_free_visibility()
  update_arp_visibility()
  params:set_action("arp_out_mode", function(_) update_arp_visibility(); Arp.hotplug_refresh() end)

  -- clock & devices
  recompute_root_midi()
  clock.run(clock_loop)

  MX.ensure_loaded(S.voice_index)
  MIDI.setup_out(1)
  Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
  -- Respect sentinel selection at boot:
  Arp.hotplug_refresh()
  setup_midi_in(params:get(S.midi.in_dev_param) or default_midi_in_index)
  rebind_midi_in_if_needed()
  redraw()
end

-- ===== input handlers =====
local k3_down_time = nil
local LONG_PRESS_SEC = 0.35

function key(n, z)
  if n == 2 and z == 1 then
    S.page = (S.page == S.PAGE_OUTPUT) and S.PAGE_CHORD or S.PAGE_OUTPUT
    redraw()
  elseif n == 3 then
    if z == 1 then
      k3_down_time = now()
    else
      local held = k3_down_time and (now() - k3_down_time) or 0
      k3_down_time = nil
      if held > LONG_PRESS_SEC then
        S.page = (S.page == S.PAGE_HUD) and S.PAGE_CHORD or S.PAGE_HUD
        redraw()
      else
        panic_all_outputs() -- K3 short press: panic
      end
    end
  end
end

function enc(n, d)
  if n == 1 then
    S.page = (S.page == S.PAGE_OUTPUT) and S.PAGE_CHORD or S.PAGE_OUTPUT
    redraw()
    return
  end

  if S.page == S.PAGE_OUTPUT then
    if n == 2 then params:delta(S.out_mode_param, d)
    elseif n == 3 then
      if want_mx() then params:delta("chorder_mx_voice", d) else params:delta(S.midi.out_dev_param, d) end
    end
    return
  end

  if S.page == S.PAGE_CHORD then
    if n == 2 then params:delta("chorder_root_pc", d)
    elseif n == 3 then params:delta("chorder_scale", d) end
    return
  end
end

function cleanup()
  if S.midi.input then S.midi.input.event = nil end
  panic_all_outputs()
end
