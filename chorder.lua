-- CHORDER — a three-voice chord-based instrument
-- Author: @abhayadana (refactor pass by ChatGPT)
-- Engine: mx.samples and/or MIDI

engine.name = "MxSamples"

local musicutil = require "musicutil"
local mxsamples = include "mx.samples/lib/mx.samples"
local chordlib  = include "lib/chordlib"
local Strum     = include "lib/strum"
local Feel      = include "lib/feel"

-- ===== config =====
local BASE = _path.audio .. "mx.samples"
local DEFAULT_MIDI_IN_NAME  = "Launchkey Mini MK3 1"
local DEFAULT_MIDI_OUT_PORT = 1

-- ===== consolidated state =====
local S = {
  -- UI pages (UPDATED ORDER: Main → Free A → Free B → Arp → Chord)
  PAGE_MAIN_IO = 1, PAGE_FREEA_IO = 2, PAGE_FREEB_IO = 3, PAGE_ARP = 4, PAGE_CHORD = 5, page = 1,

  -- cursors (for E2 select / E3 edit UI)
  cursor = { MAIN=1, FREEA=1, FREEB=1, ARP=1, CHORD=1 },

  -- root / scale
  root_pc = 0, root_oct = 1, root_midi = 24, scale_name = "Major",

  -- chord build / voicing
  chord_type = 1, sus_mode = 1, inversion = 0, spread = 0,
  voicing = 1, add_bass = false,

  -- velocity / humanize (Chord)
  velocity_mode = 1, fixed_velocity = 100,
  humanize_steps_max = 0, humanize_vel_range = 0,

  -- timing / grid (global clock for the event queue)
  quantize = false, strum_steps = 0, strum_type = 1,
  tick_div = 1/4, tick_div_str = "1/4", swing_mode = 1, swing_percent = 0, swing_phase = 0,

  -- chord state
  last_voiced_notes = nil, last_bass_note = nil,
  chord_active = {},

  -- momentary chord modifiers
  mod = { seventh=false, ninth=false, sus2=false, sus4=false, add_bass=false, mono=false, active=false, hint="" },

  -- display
  last_chord_name = nil, last_name_time = 0, last_name_timeout = 2.0,

  -- queue & tokens
  evq = {},
  trigger_seq = 0,
  chord_hold_tokens = {},

  -- output mode & levels
  out_mode_param = "chorder_out_mode",
  mx_vol_pct = 100,          -- Chord path MX volume
  arp_mx_vol_pct = 100,      -- Arp path MX volume

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

  arp_order_idx = 1,

  -- Free Play A (keeps original IDs: free_*)
  freeA = {
    id_prefix = "free",    -- param id prefix
    label = "Free A",
    enable=false, voice_index=1,
    out_mode_param = "free_out_mode",
    out_opts = {"mx.samples","mx.samples + MIDI","MIDI"},
    -- timing / strum
    strum_steps=0, strum_type=1, timing_shape=1, timing_amt=50, timing_skip_steps=1,
    swing_mode=1, swing_pct=0,
    quantize=false,
    -- velocity & gate
    velocity_mode=2, fixed_velocity=100, gate_mode=1, midi_channel=1,
    -- keying
    key_mode_param = "free_key_mode", key_mode=1,
    -- chord build / voicing
    chord_type = 1, sus_mode = 1, inversion = 0, spread = 0, voicing = 1, add_bass = 1,
    -- dynamics
    ramp_per_step = 0, accent = 1, accent_amt = 0,
    -- humanize
    hum_steps_max = 0, hum_vel_range = 0,
    -- flam
    flam = 1, flam_count = 1, flam_space = 1, flam_vel = -8,
    -- mode, transpose
    mode=1, transpose_oct=0,
    -- MIDI ports
    midi_out_dev_param = "free_midi_out_dev", midi_out_ch_param = "free_midi_out_ch",
    midi_in_ch_param  = "free_midi_in_ch",
    mout=nil,
    -- live state
    active_map={}, chord_active={}, hold_tokens={},
    last_name=nil, last_time=0,
    mx_vol_pct = 100,
    hold_to_strum = false,
  },

  -- Free Play B (new; IDs: freeb_*)
  freeB = {
    id_prefix = "freeb",
    label = "Free B",
    enable=false, voice_index=1,
    out_mode_param = "freeb_out_mode",
    out_opts = {"mx.samples","mx.samples + MIDI","MIDI"},
    -- timing / strum
    strum_steps=0, strum_type=1, timing_shape=1, timing_amt=50, timing_skip_steps=1,
    swing_mode=1, swing_pct=0,
    quantize=false,
    -- velocity & gate
    velocity_mode=2, fixed_velocity=100, gate_mode=1, midi_channel=3,  -- DEFAULT OUT CH = 3
    -- keying
    key_mode_param = "freeb_key_mode", key_mode=1,
    -- chord build / voicing
    chord_type = 1, sus_mode = 1, inversion = 0, spread = 0, voicing = 1, add_bass = 1,
    -- dynamics
    ramp_per_step = 0, accent = 1, accent_amt = 0,
    -- humanize
    hum_steps_max = 0, hum_vel_range = 0,
    -- flam
    flam = 1, flam_count = 1, flam_space = 1, flam_vel = -8,
    -- mode, transpose
    mode=1, transpose_oct=0,
    -- MIDI ports
    midi_out_dev_param = "freeb_midi_out_dev", midi_out_ch_param = "freeb_midi_out_ch",
    midi_in_ch_param  = "freeb_midi_in_ch",
    mout=nil,
    -- live state
    active_map={}, chord_active={}, hold_tokens={},
    last_name=nil, last_time=0,
    mx_vol_pct = 100,
    hold_to_strum = false,
  },
}

local function next_trigger_id() S.trigger_seq = S.trigger_seq + 1; return S.trigger_seq end

-- ===== constants =====
local K = {
  NOTE_NAMES_SHARP = {"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"},
  STRUM_OPTS = {
    "up","down","up/down","down/up","random","center-out","outside-in",
    "bass-bounce","treble-bounce","random no-repeat first","random stable ends",
    "edge-kiss","ping-pair","arp chunk 2–3","guitar rake","harp gliss split",
    "arp chunk 2–3 ↓","guitar rake ↓","harp gliss split (interleaved)",
    "bass→random","top→random","outer→random-mid","weave low↔high","weave high↔low",
    "inside-out (alt)","inside-out (random)","odds↑ then evens↑","evens↑ then odds↑",
    "low-half↑ then high-half↓",
  },
  QUANT_DIV_OPTS = {"4/1","3/1","2/1","1/1","1/2","1/3","1/4","1/6","1/8","1/12","1/16","1/24","1/32"},
  QUANT_DIV_MAP  = {
    ["4/1"]=4/1, ["3/1"]=3/1, ["2/1"]=2/1, ["1/1"]=1/1, ["1/2"]=1/2, ["1/3"]=1/3, ["1/4"]=1/4,
    ["1/6"]=1/6, ["1/8"]=1/8, ["1/12"]=1/12, ["1/16"]=1/16, ["1/24"]=1/24, ["1/32"]=1/32
  },
  WHITE_SET      = { [0]=true,[2]=true,[4]=true,[5]=true,[7]=true,[9]=true,[11]=true },
  WHITE_TO_DEG   = { [0]=1, [2]=2, [4]=3, [5]=4, [7]=5, [9]=6, [11]=7 },
}

-- === Quantize division safety ===
local function ensure_quant_div_tables()
  if not K.QUANT_DIV_OPTS or not K.QUANT_DIV_MAP then
    K.QUANT_DIV_OPTS = {"4/1","3/1","2/1","1/1","1/2","1/3","1/4","1/6","1/8","1/12","1/16","1/24","1/32"}
    K.QUANT_DIV_MAP  = {
      ["4/1"]=4/1, ["3/1"]=3/1, ["2/1"]=2/1, ["1/1"]=1/1, ["1/2"]=1/2, ["1/3"]=1/3, ["1/4"]=1/4,
      ["1/6"]=1/6, ["1/8"]=1/8, ["1/12"]=1/12, ["1/16"]=1/16, ["1/24"]=1/24, ["1/32"]=1/32
    }
  end
end

-- ===== small utils =====
local function trim(s) return (s and s:gsub("^%s*(.-)%s*$", "%1")) or "" end
local function lower(s) return string.lower(s or "") end
local function now() return util.time() end
local function param_is_on(opt_id)
  local p = params:lookup_param(opt_id)
  if not p then return false end
  local v = params:get(opt_id) or 1
  return v == 2
end
math.randomseed(os.time())

local function recompute_root_midi() S.root_midi = (S.root_oct + 1) * 12 + S.root_pc end
local function midi_to_name_oct(m) local pc=m%12; local oct=math.floor(m/12)-1; return K.NOTE_NAMES_SHARP[pc+1],oct end
local function ellipsize(s, n) s=tostring(s or ""); if #s<=n then return s end; return s:sub(1, math.max(0,n-1)).."…" end
local function short_mode_name(mode) if mode=="mx.samples" then return "mx" elseif mode=="mx.samples + MIDI" then return "mx+M" elseif mode=="MIDI" then return "MIDI" else return mode or "?" end end

-- Safe wrapper: never pass nil/empty options into params:add_option
local function safe_add_option(id, label, opts, default_index)
  if type(opts) ~= "table" or next(opts) == nil then
    opts = {"(none)"}
  end
  local count = 0
  for _ in pairs(opts) do count = count + 1 end
  local def = math.max(1, math.min(tonumber(default_index or 1) or 1, count))
  params:add_option(id, label, opts, def)
end

-- ===== BLACK KEY MOD CONFIG =====
S.black_mod_map = { [1]="7th", [3]="9th", [6]="sus2", [8]="sus4", [10]="mono" }
local BLACK_PC_LIST = {1,3,6,8,10}
local BLACK_PC_NAME = { [1]="C#", [3]="D#", [6]="F#", [8]="G#", [10]="A#" }
local MOD_OPTS = {"(none)","7th","9th","sus2","sus4","bass","mono"}
local function set_black_map_from_param(pc, idx)
  local name = MOD_OPTS[idx]
  if name == "(none)" then S.black_mod_map[pc] = nil else S.black_mod_map[pc] = name end
end

-- ===== momentary mod helpers =====
local function mod_rebuild_hint()
  local parts = {}
  if S.mod.seventh then parts[#parts+1] = "7" end
  if S.mod.ninth   then parts[#parts+1] = "9" end
  if S.mod.sus2    then parts[#parts+1] = "sus2" end
  if S.mod.sus4    then parts[#parts+1] = "sus4" end
  if S.mod.add_bass then parts[#parts+1] = "+bass" end
  if S.mod.mono    then parts[#parts+1] = "mono" end
  S.mod.hint = (#parts>0) and table.concat(parts, " ") or ""
  S.mod.active = (#parts>0)
end

local function mod_arm(kind, on)
  if kind == "7th" then
    S.mod.seventh = on
    if on then S.mod.ninth = false end
  elseif kind == "9th" then
    S.mod.ninth = on
    if on then S.mod.seventh = false end
  elseif kind == "sus2" then
    S.mod.sus2 = on
    if on then S.mod.sus4 = false end
  elseif kind == "sus4" then
    S.mod.sus4 = on
    if on then S.mod.sus2 = false end
  elseif kind == "bass" then
    S.mod.add_bass = on
  elseif kind == "mono" then
    S.mod.mono = on
  end
  mod_rebuild_hint()
  redraw()
end

local function mod_effective_build()
  local eff_chord_type = (S.mod.ninth and 3) or (S.mod.seventh and 2) or S.chord_type
  local eff_sus_mode   = (S.mod.sus2 and 2) or (S.mod.sus4 and 3) or S.sus_mode
  local eff_add_bass   = S.mod.add_bass or S.add_bass
  local eff_mono       = S.mod.mono
  return eff_chord_type, eff_sus_mode, eff_add_bass, eff_mono
end

-- ===== forward declarations =====
local rebind_midi_in_if_needed
local show_param
local setup_midi_in

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

  local function mx_on_safe_freeL(canon_name, midi_note, vel127, lane_vol_pct)
    canon_name = canon_name or ""; if canon_name=="" then return end
    local ok, err = pcall(function()
      S.mx:on({
        name = canon_name, midi = midi_note, velocity = util.clamp(vel127,1,127),
        amp = (lane_vol_pct or 100) / 100, gain = (lane_vol_pct or 100) / 100
      })
    end)
    if not ok then print("mx:on error (free lane): "..tostring(err).." (name='"..canon_name.."')") end
  end

  local function mx_on_safe_arp(canon_name, midi_note, vel127)
    canon_name = canon_name or ""; if canon_name=="" then return end
    local ok, err = pcall(function()
      S.mx:on({
        name = canon_name, midi = midi_note, velocity = util.clamp(vel127,1,127),
        amp = S.arp_mx_vol_pct / 100, gain = S.arp_mx_vol_pct / 100
      })
    end)
    if not ok then print("mx:on (arp) error: "..tostring(err).." (name='"..canon_name.."')") end
  end

  return {
    scan = scan_instruments,
    ensure_loaded = ensure_selected_loaded,
    load_folder = load_folder_into_helper,
    on_chord = mx_on_safe_chord,
    on_freeL = mx_on_safe_freeL,
    on_arp   = mx_on_safe_arp,
    off      = function(canon_name, midi_note)
      if not canon_name or canon_name=="" then return end
      local ok, err = pcall(function() S.mx:off({ name = canon_name, midi = midi_note }) end)
      if not ok then print("mx:off error: "..tostring(err).." (name='"..canon_name.."')") end
    end,
  }
end)()

-- ===== MIDI module (Awake-style) =====
local MIDI = (function()
  local function active_clear() S.midi.active_notes = {} end
  local function active_on(n) S.midi.active_notes[n] = true end
  local function active_off(n) S.midi.active_notes[n] = nil end

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

  local function build_midi_in_list()
    S.midi.in_devices = {"none"}
    S.midi.in_ports_map = { [1] = 0 }
    for i = 1, #midi.vports do
      local long_name  = midi.vports[i].name or ("dev "..i)
      local short_name = (#long_name > 15) and util.acronym(long_name) or long_name
      table.insert(S.midi.in_devices, (i..": "..short_name))
      S.midi.in_ports_map[#S.midi.in_devices] = i
    end
  end

  local function setup_out(param_index)
    local real_port = S.midi.ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
    if S.midi.out then S.midi.out.event = nil; S.midi.out = nil end
    S.midi.out = midi.connect(real_port)
    if S.midi.out then print("MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
    else print("MIDI OUT: connect failed for port "..real_port) end
  end

  local function note_on(note, vel127)
    if not S.midi.out then return end
    local vel = util.clamp(vel127, 1, 127)
    local n   = util.clamp(note,   0, 127)
    local ok, err = pcall(function() S.midi.out:note_on(n, vel, S.midi.channel) end)
    if not ok then print("MIDI OUT note_on error: "..tostring(err)) end
    active_on(n)
  end

  local function all_off()
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
        active_off(n)
      end
    end)
  end

  return {
    build_out = build_midi_device_list_awake,
    build_in  = build_midi_in_list,
    setup_out = setup_out,
    on        = note_on,
    off_all   = all_off,
    gate_for  = schedule_gate_for_note,
  }
end)()

-- ===== chord/scale helpers =====
local function scale128()
  return chordlib.build_scale(S.root_midi, S.scale_name)
end

-- ===== ARP PATTERN LIBRARY =====
local PatternLib = (function()
  local function deg(d, oct, prob, r, rest) return { d=d, oct=oct or 0, prob=prob or 1.0, r=r or 1, rest=rest or false } end
  local function clone(tbl) local t={}; for k,v in pairs(tbl) do t[k]=v end; return t end
  local function up1235(name) return { name=name, steps={deg(1),deg(2),deg(3),deg(5)} } end
  local function updown135(name) return { name=name, steps={deg(1),deg(3),deg(5),deg(3)} } end
  local function bounce15(name) return { name=name, steps={deg(1,0),deg(5,0),deg(1,1),deg(5,0)} } end
  local function walk1357(name) return { name=name, steps={deg(1),deg(3),deg(5),deg(7)} } end
  local function add_oct(template, shift)
    local t = clone(template); t.steps = {}
    for i,s in ipairs(template.steps) do t.steps[i] = { d=s.d, oct=(s.oct or 0)+(shift or 0), prob=s.prob, r=s.r, rest=s.rest } end
    t.name = template.name .. (shift>=0 and (" +"..shift.."O") or (" "..shift.."O")); return t
  end
  local function sprinkle_rests(template, every_n)
    local t = clone(template); t.steps = {}
    for i,s in ipairs(template.steps) do
      local rest = (every_n>0 and (i % every_n == 0)) or false
      t.steps[i] = { d=s.d, oct=s.oct, prob=s.prob, r=s.r, rest=rest }
    end
    t.name = template.name .. " · rests"; return t
  end
  local function probabilistic(template, p)
    local t = clone(template); t.steps = {}
    for i,s in ipairs(template.steps) do t.steps[i] = { d=s.d, oct=s.oct, prob=p, r=s.r, rest=s.rest } end
    t.name = template.name .. string.format(" · p%.2f", p); return t
  end
  local function ratchet(template, r)
    local t = clone(template); t.steps = {}
    for i,s in ipairs(template.steps) do t.steps[i] = { d=s.d, oct=s.oct, prob=s.prob, r=r, rest=s.rest } end
    t.name = template.name .. (" · x"..r); return t
  end

  local seeds = {
    Pop = {
      up1235("pop · up 1-2-3-5"), updown135("pop · 1-3-5-3"), bounce15("pop · bounce 1–5"),
      walk1357("pop · 1-3-5-7"), { name="pop · 1-6-4-5", steps={deg(1),deg(6),deg(4),deg(5)} },
      { name="pop · 1-5-6-4", steps={deg(1),deg(5),deg(6),deg(4)} },
    },
    Rock = {
      { name="rock · power walk", steps={deg(1,0),deg(5,0),deg(1,1),deg(5,0)} },
      { name="rock · 1-♭7-1-5", steps={deg(1,0),deg(7,-1),deg(1,0),deg(5,0)} },
      { name="rock · 1-4-5 climb", steps={deg(1),deg(4),deg(5),deg(1,1)} },
      updown135("rock · 1-3-5-3"),
    },
    Jazz = {
      { name="jazz · 1-3-5-7", steps={deg(1),deg(3),deg(5),deg(7)} },
      { name="jazz · 3-5-7-9", steps={deg(3),deg(5),deg(7,0),deg(2,1)} },
      { name="jazz · 7-3-5-1", steps={deg(7),deg(3),deg(5),deg(1,1)} },
      { name="jazz · 1-2-3-5", steps={deg(1),deg(2),deg(3),deg(5)} },
    },
    EDM = {
      { name="edm · saw up", steps={deg(1),deg(2),deg(3),deg(4)} },
      { name="edm · octave runner", steps={deg(1,0),deg(1,1),deg(1,0),deg(1,1)} },
      { name="edm · gate 1-5", steps={deg(1),deg(5),deg(1),deg(5,1)} },
      { name="edm · 1-3-5 ratchet", steps={deg(1),deg(3),deg(5),deg(3)} },
    },
    House = {
      { name="house · 1-3-5-6", steps={deg(1),deg(3),deg(5),deg(6)} },
      { name="house · 1-5-1+O-5", steps={deg(1),deg(5),deg(1,1),deg(5)} },
      { name="house · swing up", steps={deg(1),deg(2),deg(3),deg(2)} },
      { name="house · pluck 1-6", steps={deg(1),deg(6),deg(1),deg(6)} },
    },
    DnB = {
      { name="dnb · stabs 1", steps={deg(1),deg(1),deg(5),deg(1)} },
      { name="dnb · 1-2-♭3-5", steps={deg(1),deg(2),deg(3,0,1.0),deg(5)} },
      { name="dnb · 5-1 bounce", steps={deg(5),deg(1),deg(5),deg(1,1)} },
      { name="dnb · octave ping", steps={deg(1,1),deg(1,0),deg(1,1),deg(5,0)} },
    },
    Trap = {
      { name="trap · 1-5-6-5", steps={deg(1),deg(5),deg(6),deg(5)} },
      { name="trap · 1-5-1+O-7", steps={deg(1),deg(5),deg(1,1),deg(7)} },
      { name="trap · 1-2-5-2", steps={deg(1),deg(2),deg(5),deg(2)} },
      { name="trap · glide stairs", steps={deg(1),deg(2,1),deg(3,1),deg(5,1)} },
    },
    Ambient = {
      { name="amb · 1 • rest • 5 • rest", steps={deg(1),deg(1,0,1,1,true),deg(5),deg(5,0,1,1,true)} },
      { name="amb · 1-3 (rests)", steps={deg(1),deg(1,0,0.7,1,true),deg(3),deg(3,0,0.7,1,true)} },
      { name="amb · 1-6-5-3", steps={deg(1),deg(6),deg(5),deg(3)} },
      { name="amb · 1+O-5-3-2", steps={deg(1,1),deg(5),deg(3),deg(2)} },
    }
  }

  -- Basics patterns (folded legacy "orders")
  local function basics_patterns()
    return {
      { name="basics · use arp_order", basics=true, use_order=true },
      { name="basics · up",            basics=true, legacy="up" },
      { name="basics · down",          basics=true, legacy="down" },
      { name="basics · up/down",       basics=true, legacy="up/down" },
      { name="basics · down/up",       basics=true, legacy="down/up" },
      { name="basics · random",        basics=true, legacy="random" },
      { name="basics · center-out",    basics=true, legacy="center-out" },
      { name="basics · outside-in",    basics=true, legacy="outside-in" },
      { name="basics · random no-repeat first", basics=true, legacy="random no-repeat first" },
      { name="basics · random stable ends",     basics=true, legacy="random stable ends" },
    }
  end

  local function expand()
    local lib = {}
    for g,src in pairs(seeds) do
      lib[g] = {}
      for _,pat in ipairs(src) do
        table.insert(lib[g], pat)
        table.insert(lib[g], add_oct(pat, 1))
        table.insert(lib[g], add_oct(pat, -1))
        table.insert(lib[g], sprinkle_rests(pat, 4))
        table.insert(lib[g], probabilistic(pat, 0.85))
        table.insert(lib[g], ratchet(pat, 2))
        table.insert(lib[g], ratchet(probabilistic(pat, 0.7), 3))
      end
      for o=-1,1 do
        table.insert(lib[g], { name=(g.." · ladder 1→6 "..(o>=0 and "+"..o.."O" or o.."O")),
          steps={deg(1,o),deg(2,o),deg(3,o),deg(4,o),deg(5,o),deg(6,o),deg(5,o),deg(4,o)} })
      end
    end
    lib["Basics"] = basics_patterns()
    return lib
  end

  local LIB = expand()
  local function genres_list() local t={}; for k,_ in pairs(LIB) do t[#t+1]=k end; table.sort(t); return t end
  local function patterns_for(genre) local t = {}; if LIB[genre] then for _,p in ipairs(LIB[genre]) do t[#t+1]=p.name end end; return (#t>0) and t or {"(none)"} end
  local function get(genre, idx) local list = LIB[genre]; if not list or #list==0 then return nil end; idx = util.clamp(idx or 1, 1, #list); return list[idx] end
  local function random_for(genre) local list = LIB[genre]; if not list or #list==0 then return nil end; return list[math.random(1,#list)] end
  return { GENRES = genres_list(), NAMES_FOR = patterns_for, GET = get, RAND = random_for }
end)()

-- ===== Arp module =====
local Arp = (function()
  local A = {
    running = false,
    ord_state = { alt_flip=false, last_first=nil, last_last=nil },
    step_i = 1,
    notes_buf = {},
    emitted = {},
    coro = nil,
    mout = nil,

    -- pattern system (always ON)
    pat_genre = "Basics",
    pat_index = 1,
    pat_random_on_chord = false,
    cur_pat = nil,
    has_chord = false,
  }

  local function want_mx()
    local mval = params:get("arp_out_mode") or 1
    return (mval==1) or (mval==2)
  end
  local function want_midi()
    local mval = params:get("arp_out_mode") or 1
    return (mval==2) or (mval==3)
  end

  local function setup_midi_out(param_index_in_s_midi_devices)
    local real_port = S.midi.ports_map[param_index_in_s_midi_devices] or DEFAULT_MIDI_OUT_PORT
    if A.mout then A.mout.event = nil; A.mout = nil end
    A.mout = midi.connect(real_port)
    if A.mout then print("ARP MIDI OUT: "..(midi.vports[real_port].name or ("port "..real_port))) end
    -- keep chord/free + input alive after ARP port changes
    if rebind_midi_in_if_needed then rebind_midi_in_if_needed() end
  end

  local function fanout_on(note, vel)
    local canon = S.canonical_names[params:get("arp_mx_voice") or 1] or ""
    if want_midi() and A.mout then
      local ch = params:get("arp_midi_out_ch") or 1
      pcall(function() A.mout:note_on(util.clamp(note,0,127), util.clamp(vel,1,127), ch) end)
    end
    if want_mx() then MX.on_arp(canon, note, util.clamp(vel,1,127)) end
  end
  local function fanout_off(note)
    local canon = S.canonical_names[params:get("arp_mx_voice") or 1] or ""
    if want_midi() and A.mout then
      local ch = params:get("arp_midi_out_ch") or 1
      pcall(function() A.mout:note_off(util.clamp(note,0,127), 0, ch) end)
    end
    if want_mx() then MX.off(canon, note) end
  end
  local function all_off() for n,_ in pairs(A.emitted) do fanout_off(n) end; A.emitted = {} end

  local function scale_between(a,b, sc)
    local lo, hi = math.min(a,b), math.max(a,b)
    local out = {}
    for i=1,#sc do local v = sc[i]; if v>lo and v<hi then out[#out+1]=v end end
    table.sort(out); return out
  end

  local function make_material()
    local base_notes = {}
    if type(S.last_voiced_notes)=="table" and #S.last_voiced_notes>0 then
      for _,n in ipairs(S.last_voiced_notes) do base_notes[#base_notes+1]=n end
    else
      A.notes_buf = {}; return
    end
    table.sort(base_notes)
    local mode = params:get("arp_material") or 2 -- 1=chord only, 2=chord + passing
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

  -- map names for Basics fixed variants -> Strum indices
  local LEGACY_TO_STRUM_IDX = {
    ["up"]=1,["down"]=2,["up/down"]=3,["down/up"]=4,["random"]=5,
    ["center-out"]=6,["outside-in"]=7,
    ["random no-repeat first"]=10,["random stable ends"]=11,
  }

  local function basics_next_note(step_i)
    if not A.notes_buf or #A.notes_buf == 0 then make_material() end
    if not A.notes_buf or #A.notes_buf == 0 then return nil end

    local stype
    if A.cur_pat and A.cur_pat.use_order then
      stype = params:get("arp_order") or 1
    else
      local ord_name = (A.cur_pat and A.cur_pat.legacy) or "up"
      stype = LEGACY_TO_STRUM_IDX[ord_name] or 1
    end

    local ord = select(1, Strum.make(#A.notes_buf, stype, A.ord_state)) or {}
    if #ord == 0 then return nil end

    local idx = ord[((step_i-1) % #ord) + 1]
    local base = A.notes_buf[idx]

    local span = params:get("arp_octaves") or 1
    local walk = params:get("arp_oct_walk") or 1 -- 1=wrap, 2=bounce
    local t = math.floor((step_i-1)/#ord)
    local o
    if span <= 0 then
      o = 0
    else
      if walk == 2 then
        local period = span*2
        local p = (period==0) and 0 or (t % period)
        o = (p <= span) and p or (period - p)
      else
        o = t % (span+1)
      end
    end

    local tr = (params:get("arp_transpose_oct") or 0) * 12
    return util.clamp(base + o*12 + tr, 0, 127)
  end

  local function degree_to_midi(deg, oct_off)
    local sc = scale128()
    if not sc or #sc==0 then return nil end
    local tr_oct = (params:get("arp_transpose_oct") or 0) * 12
    local target = (S.last_bass_note or S.root_midi) + 12*(oct_off or 0) + 12 + tr_oct
    local best, bestdiff = nil, 1e9
    for i,n in ipairs(sc) do
      local d = 1 + ((i-1) % 7)
      if d == deg then
        local diff = math.abs(n - target)
        if diff < bestdiff then best, bestdiff = n, diff end
      end
    end
    return best and util.clamp(best, 0, 127) or nil
  end

  -- ROBUST: don't read params that don't exist yet
  local function select_pattern()
    local random_on = false
    local p = params:lookup_param("arp_pat_random_on_chord")
    if p then
      random_on = (params:get("arp_pat_random_on_chord") == 2)
    end
    local pat = (random_on and PatternLib.RAND(A.pat_genre))
                 or PatternLib.GET(A.pat_genre, A.pat_index)
    A.cur_pat = pat
    if pat then print("ARP pattern: "..(pat.name or "(unnamed)")) end
  end

  local function step_seconds()
    local opt = K.QUANT_DIV_OPTS[params:get("arp_div") or 7] or "1/4"
    local div = K.QUANT_DIV_MAP[opt] or 1/4
    return Feel.step_seconds(div, clock.get_tempo(), params:get("arp_swing_mode") or 1, params:get("arp_swing_pct") or 0, A.step_i)
  end

  local function play_note_with_gate(n, vel, dur)
    fanout_on(n, vel)
    A.emitted[n]=true
    if (params:get("arp_gate") or 1) > 1 then
      clock.run(function() clock.sleep(dur); if A.emitted[n] then fanout_off(n); A.emitted[n]=nil end end)
    end
  end

  local function step_once_pattern()
    all_off()
    if not A.running or not A.cur_pat then return end

    local wait = step_seconds()
    local gprob = (params:get("arp_step_prob") or 100) / 100

    local n = nil
    if A.cur_pat.basics then
      if (params:get("arp_retrack")==2) then
        local ord = select(1, Strum.make(math.max(#A.notes_buf,1), (params:get("arp_order") or 1), A.ord_state)) or {}
        if (#A.notes_buf > 0) and ((A.step_i-1) % math.max(#ord,1) == 0) then make_material() end
      end
      if math.random() <= gprob then
        n = basics_next_note(A.step_i)
      end
    else
      local steps = A.cur_pat.steps or {}
      if #steps > 0 then
        local idx = ((A.step_i-1) % #steps) + 1
        local st = steps[idx] or {}
        local prob = (st.prob or 1.0) * gprob
        local is_rest = st.rest or (math.random() > prob)
        if not is_rest and st.d then
          n = degree_to_midi(util.clamp(st.d,1,7), st.oct or 0)
        end
      end
    end

    if n then
      local gm = params:get("arp_gate") or 1
      local frac = (gm==2 and 0.25) or (gm==3 and 0.50) or (gm==4 and 0.75) or 1.0
      local vel_base = util.clamp((params:get("arp_vel") or 100) + (A.step_i-1)*(params:get("arp_vel_ramp") or 0), 1, 127)
      local hsteps = params:get("arp_hum_steps") or 0
      local hvel   = params:get("arp_hum_vel") or 0
      local pre = 0
      if hsteps > 0 then
        local sub = math.random(0, hsteps)
        pre = (sub / 8) * wait
      end
      local vel2 = util.clamp(vel_base + math.random(-hvel, hvel), 1, 127)

      local rglob = params:get("arp_step_ratchet") or 1
      local rat_total = math.max(1, math.floor(rglob))
      local rat_prob = (params:get("arp_ratchet_prob") or 100) / 100
      local sub_wait = wait / rat_total
      local sub_dur  = sub_wait * frac

      clock.run(function()
        if pre > 0 then clock.sleep(pre) end
        for j=1, rat_total do
          if j == 1 or math.random() <= rat_prob then
            play_note_with_gate(n, vel2, sub_dur)
          end
          if j < rat_total then clock.sleep(sub_wait) end
        end
      end)
    end

    A.step_i = A.step_i + 1
  end

  private = {}
  local function run()
    A.running = true
    A.step_i  = 1
    while A.running do
      local wait = step_seconds()
      local freerun = (params:get("arp_free_run") or 1) == 2
      if freerun or A.has_chord then
        if not A.cur_pat then select_pattern() end
        if A.cur_pat and A.cur_pat.basics then
          if #A.notes_buf == 0 then make_material() end
        end
        step_once_pattern()
      end
      clock.sleep(wait)
    end
  end

  local function start()
    A.running = true
    if A.coro then clock.cancel(A.coro) end
    A.coro = clock.run(run)
  end
  local function stop()
    if not A.running and not A.coro then return end
    A.running = false
    if A.coro then clock.cancel(A.coro); A.coro=nil end
    all_off()
  end

  local function chord_key(down)
    if down then
      A.has_chord = true
      select_pattern()
      A.step_i = 1
    else
      A.has_chord = false
    end
  end

  local function hotplug_refresh()
    local sel = params:get("arp_midi_out_dev") or 1
    setup_midi_out(sel)
    -- ensure chord/free MIDI + input remain bound after ARP out-mode toggles
    if rebind_midi_in_if_needed then rebind_midi_in_if_needed() end
  end

  local function set_genre_idx(i)
    local genres = (function() local t={} for _,g in ipairs(PatternLib.GENRES) do t[#t+1]=g end; table.sort(t); return t end)()
    local g = genres[i] or "Basics"
    A.pat_genre = g
    A.cur_pat=nil
    select_pattern()
  end
  local function set_pat_idx(i) A.pat_index = i or 1; A.cur_pat=nil; select_pattern() end
  local function set_rand(v) A.pat_random_on_chord = (v==2) end

  return {
    start=start, stop=stop, refresh=make_material, chord_key=chord_key,
    setup_midi_out=setup_midi_out, hotplug_refresh=hotplug_refresh,
    set_genre=set_genre_idx, set_pat=set_pat_idx, set_rand=set_rand
  }
end)()

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

-- ===== Free (lane-generic) =====
local Free = (function()
  local function lane_want_mx(L)
    local mval = params:get(L.out_mode_param) or 1
    return (mval==1) or (mval==2)
  end
  local function lane_want_midi(L)
    local mval = params:get(L.out_mode_param) or 1
    return (mval==2) or (mval==3)
  end

  local function setup_lane_midi_out(L, param_index)
    local real_port = S.midi.ports_map[param_index] or DEFAULT_MIDI_OUT_PORT
    if L.mout then L.mout.event = nil; L.mout = nil end
    L.mout = midi.connect(real_port)
    if L.mout then
      print(L.label.." MIDI OUT: "..(midi.vports[real_port].name or ("port "..real_port)))
    else
      print(L.label.." MIDI OUT: connect failed for port "..real_port)
    end
    if rebind_midi_in_if_needed then rebind_midi_in_if_needed() end
  end

  local function lane_fanout_on(L, canon, note, vel)
    if lane_want_mx(L)   then MX.on_freeL(canon, note, vel, L.mx_vol_pct) end
    if lane_want_midi(L) and L.mout then
      pcall(function() L.mout:note_on(util.clamp(note,0,127), util.clamp(vel or 100,1,127), L.midi_channel) end)
    end
  end
  local function lane_fanout_off(L, canon, note)
    if lane_want_mx(L)   then MX.off(canon, note) end
    if lane_want_midi(L) and L.mout then
      pcall(function() L.mout:note_off(util.clamp(note,0,127), 0, L.midi_channel) end)
    end
  end

  local function lane_gate_secs(L)
    local sel = L.gate_mode or 1
    if sel == 1 then return nil end
    local frac = (sel == 2 and 0.25) or (sel == 3 and 0.50) or (sel == 4 and 0.75) or 1.0
    local bpm = clock.get_tempo()
    return (60 / bpm) * (S.tick_div or 1/4) * frac
  end

  local function lane_gate_for(L, note)
    local secs = lane_gate_secs(L)
    if not secs or secs <= 0 then return end
    local n = util.clamp(note, 0, 127)
    clock.run(function()
      clock.sleep(secs)
      if L.mout then pcall(function() L.mout:note_off(n, 0, L.midi_channel) end) end
      if S.mx then local fcanon = S.canonical_names[L.voice_index] or ""; MX.off(fcanon, n) end
    end)
  end

  local function active_notes_ascending(L)
    local t = {}
    for _,q in pairs(L.active_map) do t[#t+1] = q end
    table.sort(t); return t
  end
  local function active_names(L)
    local t = active_notes_ascending(L)
    if #t == 0 then return nil end
    local parts = {}
    for _,n in ipairs(t) do local nm, oc = midi_to_name_oct(n); parts[#parts+1] = string.format("%s%d", nm, oc) end
    return table.concat(parts, " ")
  end

  local function quantize_to_scale(note)
    local sc = chordlib.build_scale(S.root_midi, S.scale_name)
    return chordlib.quantize_to_scale(sc, note)
  end

  local function make_order(L, count)
    local tmp_state = { alt_flip=false, last_first=nil, last_last=nil }
    local ord = select(1, Strum.make(count, L.strum_type, tmp_state))
    return ord or {}
  end

  local function step_offs(L, m)
    local offs = Feel.step_offsets_one_index(
      m, L.strum_steps or 0, L.timing_shape or 1, L.timing_amt or 50, L.timing_skip_steps or 1
    )
    if (L.swing_mode or 1) == 2 and (L.swing_pct or 0) > 0 then
      local s = util.clamp(L.swing_pct, 0, 75) / 100
      local skew_steps = math.floor((L.strum_steps or 0) * s + 0.5)
      if skew_steps > 0 then
        for i=1,#offs do
          if (i % 2 == 1) then offs[i] = (offs[i] or 0) + skew_steps
          else offs[i] = math.max(0, (offs[i] or 0) - skew_steps) end
        end
      end
    end
    return offs
  end

  return {
    setup_midi_out = setup_lane_midi_out,
    fanout_on  = lane_fanout_on,
    fanout_off = lane_fanout_off,
    gate_for   = lane_gate_for,
    act_names  = active_names,
    want_mx    = lane_want_mx,
    want_midi  = lane_want_midi,
    quantize_to_scale = quantize_to_scale,
    make_order = make_order,
    step_offs  = step_offs,
  }
end)()

-- ===== Event queue / clock =====
local function queue_in_steps(steps, fn) table.insert(S.evq, {steps=math.max(0, steps), fn=fn}) end
local function schedule(step_steps, fn) if not S.quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end
local function free_schedule(L, step_steps, fn) if not L.quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

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
  if S.midi and S.midi.out then
    for n,_ in pairs(S.midi.active_notes) do pcall(function() S.midi.out:note_off(n, 0, S.midi.channel) end) end
  end
  S.midi.active_notes = {}
  S.freeA.hold_tokens = {}
  S.freeB.hold_tokens = {}
end

-- ===== Chord voice handlers =====
local strum_state = { alt_flip=false, last_first=nil, last_last=nil }
local function make_strum_order(count)
  local ord, ns = Strum.make(count, S.strum_type, strum_state)
  strum_state = ns
  return ord
end

local function build_chord_and_symbol(root_note, deg)
  local sc = scale128()
  local eff_ct, eff_sus, eff_bass = mod_effective_build()
  local notes, qual, name_root_midi = chordlib.build_chord{
    scale = sc, base_midi = root_note, degree = deg,
    chord_type = eff_ct, inversion = S.inversion, spread = S.spread,
    sus_mode = eff_sus, voicing_mode = S.voicing, add_bass = eff_bass,
    last_voiced_notes = S.last_voiced_notes, last_bass_note = S.last_bass_note
  }
  local symbol = chordlib.symbol_for(notes, qual, name_root_midi, {
    chord_type = eff_ct, sus_mode = eff_sus, inversion = S.inversion,
    voicing_mode = S.voicing, add_bass = eff_bass, spread = S.spread
  })
  return notes, symbol, qual, name_root_midi
end

local function schedule_chord_strum_pass(canon, root_note, incoming_vel, deg, tid, cycle_idx, hold_guard)
  local chord = select(1, build_chord_and_symbol(root_note, deg))
  if not chord or #chord == 0 then return end

  local order = make_strum_order(#chord)
  local offs  = Feel.step_offsets_one_index(
    #order, S.strum_steps or 0,
    params:get("chorder_timing_shape") or 1,
    params:get("chorder_timing_amt")   or 50,
    params:get("chorder_timing_skip_steps") or 1
  )

  local sorted = (function(t) local c={}; for i,n in ipairs(t) do c[i]=n end; table.sort(c); return c end)(chord)
  local idx_in_sorted = {}; for i,n in ipairs(sorted) do if idx_in_sorted[n]==nil then idx_in_sorted[n]=i end end
  local n_sorted = #sorted

  local flam_on    = (params:get("chorder_flam") or 1) == 2
  local flam_cnt   = params:get("chorder_flam_count") or 0
  local flam_space = params:get("chorder_flam_space") or 1
  local flam_dvel  = params:get("chorder_flam_vel") or -8

  local pass_last = 0
  for k, idx in ipairs(order) do
    local n = chord[idx]
    local base = util.clamp((incoming_vel or 100)
                  + math.random(-(S.humanize_vel_range or 0),(S.humanize_vel_range or 0)), 1, 127)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = Feel.apply_velocity(
      k, #order, base, pos_in_sorted, n_sorted,
      params:get("chorder_ramp_per_step") or 0,
      params:get("chorder_accent") or 1,
      params:get("chorder_accent_amt") or 0
    )
    local s0 = offs[k] or 0
    local s  = s0 + math.random(0, S.humanize_steps_max or 0)
    pass_last = math.max(pass_last, s0 + (flam_on and (flam_cnt*flam_space) or 0))

    schedule(s, function()
      if (not hold_guard) or (S.chord_hold_tokens[root_note] == tid) then
        fanout_note_on(canon, n, v)
      end
    end)

    if flam_on and flam_cnt > 0 then
      for j=1, flam_cnt do
        local ds = s + j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        schedule(ds, function()
          if (not hold_guard) or (S.chord_hold_tokens[root_note] == tid) then
            fanout_note_on(canon, n, vv)
          end
        end)
      end
    end
  end

  local repeat_on = (params:get("chorder_strum_repeat") or 1) == 2
  if repeat_on then
    local gap = params:get("chorder_strum_repeat_gap") or 0
    local max_cycles = params:get("chorder_strum_repeat_cycles") or 0
    local next_idx = (cycle_idx or 1) + 1
    local should_continue = (max_cycles == 0) or (next_idx <= max_cycles)
    schedule(pass_last + gap + 1, function()
      if should_continue and S.chord_hold_tokens[root_note] == tid then
        schedule_chord_strum_pass(canon, root_note, incoming_vel, deg, tid, next_idx, hold_guard)
      end
    end)
  end
end

local function handle_note_on(canon, root_note, vel, deg)
  local _, _, _, eff_mono = mod_effective_build()
  if eff_mono then
    if S.chord_active[root_note] then
      for _,n in ipairs(S.chord_active[root_note]) do fanout_note_off(canon, n) end
      S.chord_active[root_note] = nil
    end
    S.last_voiced_notes = { root_note }
    S.last_bass_note = root_note
    S.chord_active[root_note] = { root_note }
    S.last_chord_name = (midi_to_name_oct(root_note))
    S.last_name_time = now()
    Arp.refresh()
    Arp.chord_key(true)
    fanout_note_on(canon, root_note, vel)
    return
  end

  if S.chord_active[root_note] then
    for _,n in ipairs(S.chord_active[root_note]) do fanout_note_off(canon, n) end
    S.chord_active[root_note] = nil
  end
  local chord, symbol = build_chord_and_symbol(root_note, deg)
  S.last_voiced_notes = {}; for i,n in ipairs(chord) do S.last_voiced_notes[i] = n end
  S.last_bass_note = chord[1]
  S.chord_active[root_note] = { table.unpack(chord) }
  S.last_chord_name = symbol; S.last_name_time = now()
  Arp.refresh()
  Arp.chord_key(true)

  local hold_on   = (params:get("chorder_hold_strum") or 1) == 2
  local repeat_on = (params:get("chorder_strum_repeat") or 1) == 2
  local tid = next_trigger_id()
  if hold_on or repeat_on then S.chord_hold_tokens[root_note] = tid end

  schedule_chord_strum_pass(canon, root_note, vel, deg, tid, 1, hold_on)
end

local function handle_note_off(canon, root_note)
  Arp.chord_key(false)
  if (params:get("chorder_hold_strum") or 1) == 2 then S.chord_hold_tokens[root_note] = nil end
  local notes = S.chord_active[root_note]
  if notes then
    for _,n in ipairs(notes) do fanout_note_off(canon, n) end
    S.chord_active[root_note] = nil
  end
end

-- ===== Free handlers (lane-aware) =====
local function free_handle_note_on(L, in_note, in_vel)
  local incoming = util.clamp(in_vel or 100, 1, 127)
  local base_play_vel = (L.velocity_mode == 1) and L.fixed_velocity or incoming
  local fcanon = S.canonical_names[L.voice_index] or ""

  if L.key_mode == 3 then
    local base = util.clamp(in_note + 12 * (L.transpose_oct or 0), 0, 127)
    local v = util.clamp(base_play_vel + math.random(-(L.hum_vel_range or 0),(L.hum_vel_range or 0)), 1, 127)
    L.active_map[in_note] = base
    Free.fanout_on(L, fcanon, base, v); Free.gate_for(L, base)
    return
  end

  local sc = scale128()
  local deg, base
  if L.key_mode == 1 then
    local pc = in_note % 12
    local d = K.WHITE_TO_DEG[pc]; if not d then return end
    deg = d
    local i_deg = chordlib.nearest_index_with_degree(sc, in_note, deg)
    base = sc[util.clamp(i_deg, 1, #sc)]
  else
    local qn, qi = chordlib.quantize_to_scale(sc, in_note)
    deg = 1 + ((qi - 1) % 7)
    base = qn
  end
  base = util.clamp(base + 12 * (L.transpose_oct or 0), 0, 127)

  if L.mode == 1 then
    local v = util.clamp(base_play_vel + math.random(-(L.hum_vel_range or 0),(L.hum_vel_range or 0)), 1, 127)
    L.active_map[in_note] = base
    Free.fanout_on(L, fcanon, base, v); Free.gate_for(L, base)
    return
  end

  local save_voicing, save_add = S.voicing, S.add_bass
  S.voicing, S.add_bass = L.voicing, (L.add_bass == 2)
  local notes, qual, name_root_midi = chordlib.build_chord{
    scale=sc, base_midi=base, degree=deg,
    chord_type=L.chord_type, inversion=L.inversion, spread=L.spread,
    sus_mode=L.sus_mode, voicing_mode=L.voicing, add_bass=(L.add_bass == 2),
    last_voiced_notes=nil, last_bass_note=nil
  }
  S.voicing, S.add_bass = save_voicing, save_add

  table.sort(notes)
  local ord = (#notes > 1 and L.strum_steps > 0) and Free.make_order(L, #notes) or (function(n) local t={} for i=1,n do t[i]=i end; return t end)(#notes)
  local offs = Free.step_offs(L, #ord)
  local sorted = (function(t) local c={}; for i,n in ipairs(t) do c[i]=n end; table.sort(c); return c end)(notes)
  local idx_in_sorted = {}; for i,n in ipairs(sorted) do if idx_in_sorted[n]==nil then idx_in_sorted[n]=i end end
  local n_sorted = #sorted

  local hold_on = L.hold_to_strum or param_is_on(L.id_prefix.."_hold_strum")
  local tid = next_trigger_id()
  if hold_on then L.hold_tokens[in_note] = tid end

  local emitted = {}
  for k, idx in ipairs(ord) do
    local n = notes[idx]
    local s = (offs[k] or 0) + math.random(0, L.hum_steps_max or 0)
    local v_base = util.clamp(base_play_vel + math.random(-(L.hum_vel_range or 0), (L.hum_vel_range or 0)), 1, 127)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = Feel.apply_velocity(
      k, #ord, v_base, pos_in_sorted, n_sorted,
      L.ramp_per_step or 0,
      L.accent or 1,
      L.accent_amt or 0
    )
    free_schedule(L, s, function()
      if (not hold_on) or (L.hold_tokens[in_note] == tid) then Free.fanout_on(L, fcanon, n, v); Free.gate_for(L, n) end
    end)
    local flam_on = (params:get(L.id_prefix.."_flam") or L.flam) == 2
    if flam_on then
      local flam_cnt   = params:get(L.id_prefix.."_flam_count") or L.flam_count or 0
      local flam_space = params:get(L.id_prefix.."_flam_space") or L.flam_space or 1
      local flam_dvel  = params:get(L.id_prefix.."_flam_vel") or L.flam_vel or -8
      for j=1, flam_cnt do
        local ds = s + j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        free_schedule(L, ds, function()
          if (not hold_on) or (L.hold_tokens[in_note] == tid) then Free.fanout_on(L, fcanon, n, vv); Free.gate_for(L, n) end
        end)
      end
    end
    emitted[#emitted + 1] = n
  end
  L.chord_active[in_note] = emitted

  local symbol = chordlib.symbol_for(notes, qual, name_root_midi, {
    chord_type=L.chord_type, sus_mode=L.sus_mode, inversion=L.inversion,
    voicing_mode=L.voicing, add_bass=(L.add_bass == 2), spread=L.spread
  })
  L.last_name = symbol
  L.last_time = util.time()
end

local function free_handle_note_off(L, in_note)
  if L.mode == 1 or L.key_mode == 3 then
    local q = L.active_map[in_note]
    if q ~= nil then
      L.active_map[in_note] = nil
      local fcanon = S.canonical_names[L.voice_index] or ""
      Free.fanout_off(L, fcanon, q)
    end
    return
  end
  if L.hold_to_strum or param_is_on(L.id_prefix.."_hold_strum") then L.hold_tokens[in_note] = nil end
  local outs = L.chord_active[in_note]
  if outs then
    local fcanon = S.canonical_names[L.voice_index] or ""
    for _,n in ipairs(outs) do Free.fanout_off(L, fcanon, n) end
    L.chord_active[in_note] = nil
  end
end

-- ===== UI helpers =====
local function draw_header(active_label)
  screen.level(15); screen.move(4, 12); screen.text("CHORDER")
  screen.level(10); screen.move(124, 12); screen.text_right(active_label and ("["..active_label.."]") or "")
end
local function draw_line(y, label, value)
  screen.level(12); screen.move(10, y); screen.text(label or "")
  screen.level(15); screen.move(124, y); screen.text_right(value or "")
end
local function key_center_string()
  local name = K.NOTE_NAMES_SHARP[S.root_pc+1]; local rname, roct = midi_to_name_oct(S.root_midi)
  return string.format("%s (root %s%d)", name, rname, roct)
end

local function caret_at(line_y, selected) if selected then screen.level(15); screen.move(2, line_y); screen.text(">") end end
local function clamp_cursor(cur, n) return util.clamp(cur or 1, 1, math.max(1, n or 1)) end
local function short_mode_from_param(id, opts_tbl) local full = opts_tbl[(params:get(id) or 1)] or "?" return short_mode_name(full) end
local function current_midi_out_label(idx) return ellipsize(S.midi.devices[idx or (params:get(S.midi.out_dev_param) or 1)] or "—", 16) end
local function current_midi_in_label() local i = params:get(S.midi.in_dev_param) or 1; return ellipsize(S.midi.in_devices[i] or "none", 16) end
local function channel_disp(idx) return (idx==1) and "Omni" or ("Ch"..tostring((idx or 2)-1)) end

-- ===== Page items =====

local function items_main_io()
  local items = {}

  -- In (Chord input & ch)
  table.insert(items, {
    label = "In",
    value_str = function()
      local in_lbl = current_midi_in_label()
      local in_chi = params:get(S.midi.in_ch_param) or 1
      return in_lbl.." / "..channel_disp(in_chi)
    end,
    delta = function(d)
      if math.abs(d) == 1 then
        params:delta(S.midi.in_dev_param, d)
        setup_midi_in(params:get(S.midi.in_dev_param) or 1)
      else
        params:delta(S.midi.in_ch_param, d > 0 and 1 or -1)
      end
    end
  })

  -- Out mode
  table.insert(items, {
    label = "Out",
    value_str = function()
      return short_mode_from_param(S.out_mode_param, OUT_OPTS)
    end,
    delta = function(d) params:delta(S.out_mode_param, d) end
  })

  do
    local out_short = short_mode_from_param(S.out_mode_param, OUT_OPTS)
    if out_short == "mx" or out_short == "mx+M" then
      table.insert(items, {
        label = "mx.samples",
        value_str = function()
          return ellipsize(S.display_names[params:get("chorder_mx_voice") or S.voice_index] or "(no packs)", 20)
        end,
        delta = function(d) params:delta("chorder_mx_voice", d) end
      })
      table.insert(items, {
        label = "mx vol",
        value_str = function() return tostring(params:get("chorder_mx_vol_pct") or S.mx_vol_pct).."%" end,
        delta = function(d) params:delta("chorder_mx_vol_pct", d > 0 and 1 or -1) end
      })
    end
    if out_short == "MIDI" or out_short == "mx+M" then
      table.insert(items, {
        label = "MIDI Out",
        value_str = function()
          local dev = current_midi_out_label(params:get(S.midi.out_dev_param))
          local ch  = tostring(params:get(S.midi.out_ch_param) or 1)
          return dev.." / Ch"..ch
        end,
        delta = function(d)
          if math.abs(d) == 1 then params:delta(S.midi.out_dev_param, d)
          else params:delta(S.midi.out_ch_param, d > 0 and 1 or -1) end
        end
      })
    end
  end

  -- Velocity source (Chord)
  table.insert(items, {
    label = "Vel src",
    value_str = function()
      local p = params:lookup_param("chorder_vel_mode")
      return (p and p.options and p.options[params:get("chorder_vel_mode")]) or "fixed"
    end,
    delta = function(d)
      params:delta("chorder_vel_mode", d)
      local i = params:get("chorder_vel_mode")
      show_param("chorder_vel_fixed", i == 1)
    end
  })

  return items
end

local function items_free_io(L)
  local items = {}

  table.insert(items, {
    label = L.label,
    value_str = function() return (params:get(L.id_prefix.."_enable")==2) and "on" or "off" end,
    delta = function(d) params:delta(L.id_prefix.."_enable", d) end
  })

  table.insert(items, {
    label = "In Ch",
    value_str = function() return tostring(params:get(L.midi_in_ch_param) or 2) end,
    delta = function(d) params:delta(L.midi_in_ch_param, d > 0 and 1 or -1) end
  })

  table.insert(items, {
    label = "Out",
    value_str = function()
      return short_mode_from_param(L.out_mode_param, L.out_opts)
    end,
    delta = function(d) params:delta(L.out_mode_param, d) end
  })

  do
    local out_short = short_mode_from_param(L.out_mode_param, L.out_opts)
    if out_short == "mx" or out_short == "mx+M" then
      table.insert(items, {
        label = "mx.samples",
        value_str = function()
          return ellipsize(S.display_names[params:get(L.id_prefix.."_mx_voice") or L.voice_index] or "(no packs)", 20)
        end,
        delta = function(d) params:delta(L.id_prefix.."_mx_voice", d) end
      })
      table.insert(items, {
        label = "mx vol",
        value_str = function() return tostring(params:get(L.id_prefix.."_mx_vol_pct") or L.mx_vol_pct).."%" end,
        delta = function(d) params:delta(L.id_prefix.."_mx_vol_pct", d > 0 and 1 or -1) end
      })
    end
    if out_short == "MIDI" or out_short == "mx+M" then
      table.insert(items, {
        label = "MIDI Out",
        value_str = function()
          local dev = current_midi_out_label(params:get(L.midi_out_dev_param))
          local ch  = tostring(params:get(L.midi_out_ch_param) or L.midi_channel)
          return dev.." / Ch"..ch
        end,
        delta = function(d)
          if math.abs(d) == 1 then
            params:delta(L.midi_out_dev_param, d)
            Free.setup_midi_out(L, params:get(L.midi_out_dev_param) or 1)
          else
            params:delta(L.midi_out_ch_param, d > 0 and 1 or -1)
          end
        end
      })
    end
  end

  return items
end

local function items_arp_io()
  local items = {}

  table.insert(items, {
    label = "Arp",
    value_str = function() return (params:get("arp_enable")==2) and "on" or "off" end,
    delta = function(d) params:delta("arp_enable", d) end
  })

  table.insert(items, {
    label = "Out",
    value_str = function()
      local out_opts = {"mx.samples","mx.samples + MIDI","MIDI"}
      local full = out_opts[params:get("arp_out_mode") or 1] or "?"
      return short_mode_name(full)
    end,
    delta = function(d) params:delta("arp_out_mode", d) end
  })

  do
    local out_opts = {"mx.samples","mx.samples + MIDI","MIDI"}
    local full = out_opts[params:get("arp_out_mode") or 1] or "?"
    local out_short = short_mode_name(full)
    if out_short == "mx" or out_short == "mx+M" then
      table.insert(items, {
        label = "mx.samples",
        value_str = function()
          return ellipsize(S.display_names[params:get("arp_mx_voice") or 1] or "(no packs)", 20)
        end,
        delta = function(d) params:delta("arp_mx_voice", d) end
      })
      table.insert(items, {
        label = "mx vol",
        value_str = function() return tostring(params:get("arp_mx_vol_pct") or S.arp_mx_vol_pct).."%" end,
        delta = function(d) params:delta("arp_mx_vol_pct", d > 0 and 1 or -1) end
      })
    end
    if out_short == "MIDI" or out_short == "mx+M" then
      table.insert(items, {
        label = "MIDI Out",
        value_str = function()
          local dev = current_midi_out_label(params:get("arp_midi_out_dev"))
          local ch  = tostring(params:get("arp_midi_out_ch") or 1)
          return dev.." / Ch"..ch
        end,
        delta = function(d)
          if math.abs(d) == 1 then
            params:delta("arp_midi_out_dev", d)
            Arp.setup_midi_out(params:get("arp_midi_out_dev") or 1)
          else
            params:delta("arp_midi_out_ch", d > 0 and 1 or -1)
          end
        end
      })
    end
  end

  return items
end

local function items_chord_page()
  local function option_label(id)
    local p = params:lookup_param(id)
    if not p or not p.options then return "?" end
    local i = params:get(id) or 1
    return p.options[i] or "?"
  end

  return {
    {
      label="Key",
      value_str=function() return key_center_string() end,
      delta=function(d) params:delta("chorder_root_pc", d) end
    },
    {
      label="Scale",
      value_str=function() return S.scale_name end,
      delta=function(d) params:delta("chorder_scale", d) end
    },
    {
      label="Voicing",
      value_str=function() return option_label("chorder_voicing") end,
      delta=function(d) params:delta("chorder_voicing", d) end
    },
    {
      label="Add Bass",
      value_str=function() return option_label("chorder_bass_note") end,
      delta=function(d) params:delta("chorder_bass_note", d > 0 and 1 or -1) end
    },
  }
end

-- ===== Page draw =====
local function draw_items_list(y_start, items, cursor_idx)
  local y = y_start
  for i, it in ipairs(items) do
    local selected = (i == cursor_idx)
    caret_at(y, selected)
    draw_line(y, it.label..":", it.value_str and it.value_str() or "")
    y = y + 8
  end
end

local function draw_main_io_page()
  draw_header("Main Chord I/O")
  local items = items_main_io()
  S.cursor.MAIN = clamp_cursor(S.cursor.MAIN, #items)
  draw_items_list(24, items, S.cursor.MAIN)
end

local function draw_free_io_page(L, cursor_key)
  draw_header(L.label.." I/O")
  local items = items_free_io(L)
  S.cursor[cursor_key] = clamp_cursor(S.cursor[cursor_key], #items)
  draw_items_list(24, items, S.cursor[cursor_key])
end

local function draw_arp_io_page()
  draw_header("Arp I/O")
  local items = items_arp_io()
  S.cursor.ARP = clamp_cursor(S.cursor.ARP, #items)
  draw_items_list(24, items, S.cursor.ARP)
end

local function draw_chord_page()
  draw_header("Chord")
  local items = items_chord_page()
  S.cursor.CHORD = clamp_cursor(S.cursor.CHORD, #items)
  draw_items_list(24, items, S.cursor.CHORD)

  if S.mod.active and S.mod.hint ~= "" then
    screen.level(9); screen.move(64, 55); screen.text_center("mods: "..S.mod.hint)
  end

  local now_t = now()
  local show_name = S.last_chord_name and ((now_t - S.last_name_time) < S.last_name_timeout)
  screen.level(show_name and 15 or 10)
  screen.move(64, 62); screen.text_center(show_name and ellipsize(S.last_chord_name, 26) or "(play a chord)")
end

function redraw()
  screen.clear()
  if     S.page == S.PAGE_MAIN_IO  then draw_main_io_page()
  elseif S.page == S.PAGE_FREEA_IO then draw_free_io_page(S.freeA, "FREEA")
  elseif S.page == S.PAGE_FREEB_IO then draw_free_io_page(S.freeB, "FREEB")
  elseif S.page == S.PAGE_ARP      then draw_arp_io_page()
  else                                   draw_chord_page()
  end
  screen.update()
end

-- ===== group-builder =====
local function build_group(title, builders)
  local real = params
  local counter = 0
  local proxy = {}
  local function bump() counter = counter + 1 end
  proxy.add_option   = function(...) bump() end
  proxy.add_number   = function(...) bump() end
  proxy.add_trigger  = function(...) bump() end
  proxy.add_text     = function(...) bump() end
  proxy.add_binary   = function(...) bump() end
  proxy.add_control  = function(...) bump() end
  proxy.add_separator= function(...) bump() end
  proxy.set_action   = function(...) end
  proxy.hide         = function(...) end
  proxy.show         = function(...) end
  proxy.lookup_param = function(...) return nil end
  setmetatable(proxy, { __index = function() return function(...) end end })

  params = proxy
  for _,fn in ipairs(builders) do fn() end
  params = real

  params:add_group(title, counter)
  for _,fn in ipairs(builders) do fn() end
end

local function div(label)
  return function() params:add_separator(("— %s —"):format(label or "")) end
end

-- ===== MIDI In setup =====
local function teardown_midi_in()
  if S.midi.input then S.midi.input.event = nil; S.midi.input = nil end
end

setup_midi_in = function(param_index)
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

    local chord_ok
    do
      local ch_sel_idx = params:get(S.midi.in_ch_param)
      chord_ok = (ch_sel_idx == 1) or (msg.ch == (ch_sel_idx - 1))
    end

    -- FREE LANE routing by their input channels
    local freeA_ok = false
    local freeB_ok = false
    do
      local a_ch = params:get(S.freeA.midi_in_ch_param) or 2
      local b_ch = params:get(S.freeB.midi_in_ch_param) or 2  -- change default to 3 if you want Free B input on ch3 by default
      freeA_ok = (msg.ch == a_ch)
      freeB_ok = (msg.ch == b_ch)
    end

    local canon = S.canonical_names[S.voice_index] or ""
    local chord_vel = (S.velocity_mode == 2 and (msg.vel or S.fixed_velocity) or S.fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      if chord_ok then
        local pc = msg.note % 12
        if not K.WHITE_SET[pc] then
          local m = S.black_mod_map[pc]
          if m then mod_arm(m, true) end
          return
        end
        local deg = K.WHITE_TO_DEG[pc]
        if deg then
          MX.ensure_loaded(S.voice_index)
          handle_note_on(canon, msg.note, chord_vel, deg)
        end
      end

      if S.freeA.enable and freeA_ok then
        local vel = (S.freeA.velocity_mode == 1) and S.freeA.fixed_velocity or msg.vel
        free_handle_note_on(S.freeA, msg.note, vel)
      end
      if S.freeB.enable and freeB_ok then
        local vel = (S.freeB.velocity_mode == 1) and S.freeB.fixed_velocity or msg.vel
        free_handle_note_on(S.freeB, msg.note, vel)
      end

      redraw()

    elseif (msg.type == "note_off") or (msg.type == "note_on" and msg.vel == 0) then
      if chord_ok then
        local pc = msg.note % 12
        if K.WHITE_SET[pc] then
          handle_note_off(canon, msg.note)
        else
          local m = S.black_mod_map[pc]
          if m then mod_arm(m, false) end
        end
      end
      if S.freeA.enable and freeA_ok then free_handle_note_off(S.freeA, msg.note) end
      if S.freeB.enable and freeB_ok then free_handle_note_off(S.freeB, msg.note) end
      redraw()
    end
  end

  print("MIDI IN: connected to "..(S.midi.in_devices[param_index] or ("port "..port)))
end

rebind_midi_in_if_needed = function()
  if (S.midi.input == nil) or (S.midi.input.event == nil) then
    local cur = params:get(S.midi.in_dev_param) or 1
    setup_midi_in(cur)
  end
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

  -- Rebind option lists for chord/arp/free lanes
  params:hide(S.midi.in_dev_param);  params:show(S.midi.in_dev_param)
  local p_in = params:lookup_param(S.midi.in_dev_param)
  if p_in then p_in.options = S.midi.in_devices; p_in.count = #S.midi.in_devices end

  params:hide(S.midi.out_dev_param); params:show(S.midi.out_dev_param)
  local p_out = params:lookup_param(S.midi.out_dev_param)
  if p_out then p_out.options = S.midi.devices; p_out.count = #S.midi.devices end

  params:hide(S.freeA.midi_out_dev_param); params:show(S.freeA.midi_out_dev_param)
  local p_free_outA = params:lookup_param(S.freeA.midi_out_dev_param)
  if p_free_outA then p_free_outA.options = S.midi.devices; p_free_outA.count = #S.midi.devices end

  params:hide(S.freeB.midi_out_dev_param); params:show(S.freeB.midi_out_dev_param)
  local p_free_outB = params:lookup_param(S.freeB.midi_out_dev_param)
  if p_free_outB then p_free_outB.options = S.midi.devices; p_free_outB.count = #S.midi.devices end

  params:hide("arp_midi_out_dev"); params:show("arp_midi_out_dev")
  local p_arp_out = params:lookup_param("arp_midi_out_dev")
  if p_arp_out then
    p_arp_out.options = S.midi.devices
    p_arp_out.count   = #S.midi.devices
    local cur = params:get("arp_midi_out_dev") or 1
    if cur > p_arp_out.count then params:set("arp_midi_out_dev", p_arp_out.count) end
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

-- ===== lifecycle utils =====
show_param = function(id, show)
  local p=params:lookup_param(id)
  if not p then return end
  if show then params:show(id) else params:hide(id) end
end

local function update_free_visibility(L)
  local chroma = (L.key_mode == 3)
  show_param(L.id_prefix.."_mode",                not chroma)
  show_param(L.id_prefix.."_chord_type",          not chroma)
  show_param(L.id_prefix.."_sus_mode",            not chroma)
  show_param(L.id_prefix.."_inversion",           not chroma)
  show_param(L.id_prefix.."_spread",              not chroma)
  show_param(L.id_prefix.."_voicing",             not chroma)
  show_param(L.id_prefix.."_add_bass",            not chroma)
  show_param(L.id_prefix.."_strum_steps",         not chroma)
  show_param(L.id_prefix.."_strum_type",          not chroma)
  show_param(L.id_prefix.."_hold_strum",          not chroma)
  show_param(L.id_prefix.."_vel_fixed",           true)
  show_param(L.id_prefix.."_gate_mode",           true)
end

local function update_arp_basics_visibility()
  local pg = params:lookup_param("arp_pat_genre")
  local is_basics = false
  if pg and pg.options then
    local gname = pg.options[params:get("arp_pat_genre") or 1]
    is_basics = (gname == "Basics")
  end
  show_param("arp_order",   is_basics)
  show_param("arp_octaves", is_basics)
  show_param("arp_oct_walk",is_basics)
end

-- ===== sections =====
local function add_io_section(default_midi_in_index)
  build_group("CHORDER · I/O", {
    div("I/O · Output"),
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

    div("I/O · MIDI Out"),
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

    div("I/O · MIDI In"),
    function()
      params:add_option(S.midi.in_dev_param, "MIDI input", S.midi.in_devices, default_midi_in_index)
      params:set_action(S.midi.in_dev_param, function(i) setup_midi_in(i) end)
    end,
    function()
      local ch_opts_in = {"omni"}; for i=1,16 do ch_opts_in[#ch_opts_in+1]=tostring(i) end
      params:add_option(S.midi.in_ch_param, "chord MIDI input ch", ch_opts_in, 2)
    end,

    div("I/O · Instruments (Chord)"),
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
        for _, lane in ipairs({S.freeA, S.freeB}) do
          local pid = params:lookup_param(lane.id_prefix.."_mx_voice")
          if pid then
            pid.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
            pid.count   = (#S.display_names>0 and #S.display_names or 1)
            lane.voice_index = 1; params:set(lane.id_prefix.."_mx_voice", lane.voice_index)
          end
        end
        local pa = params:lookup_param("arp_mx_voice")
        if pa then
          pa.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
          pa.count   = (#S.display_names>0 and #S.display_names or 1)
        end
        redraw()
      end)
    end,

    div("I/O · Levels"),
    function()
      params:add_number("chorder_mx_vol_pct", "mx volume (%)", 0, 200, S.mx_vol_pct)
      params:set_action("chorder_mx_vol_pct", function(v) S.mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,
  })
end

local function add_setup_section()
  local VOICE_OPTS = { "none","drop-2","drop-3","drop-2&4","drop-1", "open","wide","quartal","quintal","nearest","smooth" }

  build_group("CHORDER · Musical Setup", {
    div("Setup · Key & Scale"),
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

    div("Setup · Chord Build"),
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

    div("Setup · Voicing & Bass"),
    function()
      params:add_option("chorder_voicing", "voicing", VOICE_OPTS, S.voicing)
      params:set_action("chorder_voicing", function(i) S.voicing = i; show_param("chorder_inversion", S.voicing ~= 11); redraw() end)
    end,
    function()
      params:add_option("chorder_bass_note", "add bass (root -12)", {"off","on"}, (S.add_bass and 2 or 1))
      params:set_action("chorder_bass_note", function(i) S.add_bass = (i==2); redraw() end)
    end,

    div("Setup · Velocity (Chord)"),
    function()
      params:add_option("chorder_vel_mode", "velocity src (chord)", {"fixed","incoming"}, S.velocity_mode)
      params:set_action("chorder_vel_mode", function(i) S.velocity_mode = i; show_param("chorder_vel_fixed", i==1) end)
    end,
    function()
      params:add_number("chorder_vel_fixed", "fixed velocity (chord)", 1, 127, S.fixed_velocity)
      params:set_action("chorder_vel_fixed", function(v) S.fixed_velocity = util.clamp(v,1,127) end)
    end,

    div("Setup · Black-key mods"),
    function()
      for _,pc in ipairs(BLACK_PC_LIST) do
        local param_id = "bkmod_"..pc
        local def_idx = 1
        local cur = S.black_mod_map[pc] or "(none)"
        for i,n in ipairs(MOD_OPTS) do if n == cur then def_idx = i break end end
        params:add_option(param_id, BLACK_PC_NAME[pc].." mod", MOD_OPTS, def_idx)
        params:set_action(param_id, function(i) set_black_map_from_param(pc, i) end)
      end
    end,
  })
end

local function add_timing_section()
  build_group("CHORDER · Timing & Feel", {
    div("Timing · Clock & Grid"),
    function()
      params:add_option("chorder_quantize", "quantize chords", {"off","on"}, (S.quantize and 2 or 1))
      params:set_action("chorder_quantize", function(i) S.quantize = (i==2); redraw() end)
    end,
    function()
      ensure_quant_div_tables(); safe_add_option("chorder_quant_div", "quantize division", K.QUANT_DIV_OPTS, 7)
      params:set_action("chorder_quant_div", function(i)
        ensure_quant_div_tables()
        S.tick_div_str = K.QUANT_DIV_OPTS[i] or "1/4"
        S.tick_div = K.QUANT_DIV_MAP[S.tick_div_str] or 1/4
        redraw()
      end)
    end,
    function()
      params:add_option("chorder_swing_mode", "swing mode", {"grid","swing %"}, S.swing_mode)
      params:set_action("chorder_swing_mode", function(i) S.swing_mode = i; show_param("chorder_swing_pct", S.swing_mode == 2); redraw() end)
    end,
    function()
      params:add_number("chorder_swing_pct", "swing %", 0, 75, S.swing_percent)
      params:set_action("chorder_swing_pct", function(v) S.swing_percent = util.clamp(v, 0, 75); redraw() end)
    end,

    div("Timing · Gate (Chord MIDI)"),
    function() params:add_option(S.midi.gate_param, "MIDI gate", S.midi.gate_opts, 1) end,

    div("Timing · Strum"),
    function()
      params:add_number("chorder_strum", "strum (steps of division)", 0, 8, S.strum_steps)
      params:set_action("chorder_strum", function(v) S.strum_steps = util.clamp(v,0,8); redraw() end)
    end,
    function()
      params:add_option("chorder_strum_type", "strum type", K.STRUM_OPTS, S.strum_type)
      params:set_action("chorder_strum_type", function(i)
        S.strum_type = i
        strum_state = { alt_flip=false, last_first=nil, last_last=nil }
        redraw()
      end)
    end,
    function() params:add_option("chorder_hold_strum", "hold-to-strum (Chord)", {"off","on"}, 1) end,
    function() params:add_option("chorder_strum_repeat", "repeat strum", {"off","on"}, 1) end,
    function() params:add_number("chorder_strum_repeat_gap", "repeat gap (steps)", 0, 8, 0) end,
    function() params:add_number("chorder_strum_repeat_cycles", "repeat cycles (0=inf)", 0, 32, 0) end,

    div("Timing · Timing Shapes"),
    function()
      params:add_option("chorder_timing_shape", "timing shape",
        {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, 1)
    end,
    function() params:add_number("chorder_timing_amt", "timing amount", 0, 100, 50) end,
    function() params:add_number("chorder_timing_skip_steps", "skip size (steps)", 0, 8, 1) end,

    div("Timing · Humanize"),
    function()
      params:add_number("chorder_hum_steps", "humanize timing (max steps)", 0, 4, S.humanize_steps_max)
      params:set_action("chorder_hum_steps", function(v) S.humanize_steps_max = util.clamp(v,0,4); redraw() end)
    end,
    function()
      params:add_number("chorder_hum_vel", "humanize velocity (+/-)", 0, 30, S.humanize_vel_range)
      params:set_action("chorder_hum_vel", function(v) S.humanize_vel_range = util.clamp(v,0,30); redraw() end)
    end,

    div("Timing · Dynamics"),
    function() params:add_number("chorder_ramp_per_step", "velocity ramp / step", -24, 24, -6) end,
    function() params:add_option("chorder_accent", "accent target", {"none","bass","top","middle"}, 2) end,
    function() params:add_number("chorder_accent_amt", "accent amount", -30, 30, 12) end,

    div("Timing · Flam"),
    function() params:add_option("chorder_flam", "flam", {"off","on"}, 1) end,
    function() params:add_number("chorder_flam_count", "flam hits (extra)", 0, 3, 1) end,
    function() params:add_number("chorder_flam_space", "flam spacing (steps)", 1, 4, 1) end,
    function() params:add_number("chorder_flam_vel", "flam vel delta", -30, 0, -8) end,
  })
end

local function add_free_lane_section(L, defaults)
  build_group("CHORDER · "..L.label, {
    div(L.label.." · Toggle"),
    function()
      params:add_option(L.id_prefix.."_enable", "free play", {"off","on"}, 1)
      params:set_action(L.id_prefix.."_enable", function(i) L.enable = (i==2); if not L.enable then panic_all_outputs() end; redraw() end)
    end,

    div(L.label.." · MIDI In"),
    function()
      params:add_option(L.midi_in_ch_param, "MIDI input ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), defaults.in_ch or 2)
    end,

    div(L.label.." · Key Input Mode"),
    function()
      params:add_option(L.key_mode_param, "",
        {"white keys (diatonic degrees)","all keys → quantized to scale","all keys chromatic (mono only)"}, L.key_mode)
      params:set_action(L.key_mode_param, function(i)
        L.key_mode = i
        if L.key_mode == 3 then
          params:set(L.id_prefix.."_mode", 1); L.mode = 1; panic_all_outputs()
        end
        update_free_visibility(L); redraw()
      end)
    end,

    div(L.label.." · Instrument & Output"),
    function()
      params:add_option(L.out_mode_param, "output", L.out_opts, 1)
      params:set_action(L.out_mode_param, function(_) panic_all_outputs(); rebind_midi_in_if_needed(); show_param(L.id_prefix.."_gate_mode", true); redraw() end)
    end,
    function()
      params:add_option(L.id_prefix.."_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action(L.id_prefix.."_mx_voice", function(i) L.voice_index = i; MX.ensure_loaded(i); redraw() end)
    end,

    div(L.label.." · MIDI Out"),
    function()
      params:add_option(L.midi_out_dev_param, "MIDI out", S.midi.devices, 1)
      params:set_action(L.midi_out_dev_param, function(i) Free.setup_midi_out(L, i); panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,
    function()
      params:add_option(L.midi_out_ch_param, "MIDI out ch", (function() local t={} for i=1,16 do t[i]=tostring(i) end; return t end)(), defaults.out_ch or 1)
      params:set_action(L.midi_out_ch_param, function(idx) L.midi_channel = idx; panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,

    div(L.label.." · Mode"),
    function()
      params:add_option(L.id_prefix.."_mode", "play mode", {"mono","chord"}, L.mode)
      params:set_action(L.id_prefix.."_mode", function(i) L.mode = i end)
    end,

    div(L.label.." · Chord Build"),
    function() params:add_option(L.id_prefix.."_chord_type", "chord type", {"triad","7th","9th"}, L.chord_type); params:set_action(L.id_prefix.."_chord_type", function(i) L.chord_type = i end) end,
    function() params:add_option(L.id_prefix.."_sus_mode", "third handling", {"normal","sus2","sus4"}, L.sus_mode); params:set_action(L.id_prefix.."_sus_mode", function(i) L.sus_mode = i end) end,
    function() params:add_number(L.id_prefix.."_inversion", "inversion (0-3)", 0, 3, L.inversion); params:set_action(L.id_prefix.."_inversion", function(v) L.inversion = util.clamp(v,0,3) end) end,
    function() params:add_number(L.id_prefix.."_spread", "spread (semitones)", -24, 24, L.spread); params:set_action(L.id_prefix.."_spread", function(v) L.spread = util.round(v) end) end,
    function()
      params:add_option(L.id_prefix.."_voicing", "voicing",
        {"none","drop-2","drop-3","drop-2&4","drop-1","open","wide","quartal","quintal","nearest","smooth"}, L.voicing)
      params:set_action(L.id_prefix.."_voicing", function(i) L.voicing = i end)
    end,
    function()
      params:add_option(L.id_prefix.."_add_bass", "add bass (root -12)", {"off","on"}, L.add_bass)
      params:set_action(L.id_prefix.."_add_bass", function(i) L.add_bass = i end)
    end,

    div(L.label.." · Strum"),
    function() params:add_number(L.id_prefix.."_strum_steps", "strum (steps of division)", 0, 8, L.strum_steps); params:set_action(L.id_prefix.."_strum_steps", function(v) L.strum_steps = util.clamp(v,0,8) end) end,
    function() params:add_option(L.id_prefix.."_strum_type", "strum type", K.STRUM_OPTS, L.strum_type)
      params:set_action(L.id_prefix.."_strum_type", function(i) L.strum_type = i end)
    end,
    function()
      params:add_option(L.id_prefix.."_hold_strum", "hold-to-strum", {"off","on"}, 1)
      params:set_action(L.id_prefix.."_hold_strum", function(i) L.hold_to_strum = (i == 2) end)
    end,

    div(L.label.." · Dynamics"),
    function() params:add_number(L.id_prefix.."_ramp_per_step", "velocity ramp / step", -24, 24, L.ramp_per_step)
      params:set_action(L.id_prefix.."_ramp_per_step", function(v) L.ramp_per_step = util.clamp(v, -24, 24) end)
    end,
    function() params:add_option(L.id_prefix.."_accent", "accent target", {"none","bass","top","middle"}, L.accent)
      params:set_action(L.id_prefix.."_accent", function(i) L.accent = i end)
    end,
    function() params:add_number(L.id_prefix.."_accent_amt", "accent amount", -30, 30, L.accent_amt)
      params:set_action(L.id_prefix.."_accent_amt", function(v) L.accent_amt = util.clamp(v, -30, 30) end)
    end,

    div(L.label.." · Flam"),
    function() params:add_option(L.id_prefix.."_flam", "flam", {"off","on"}, L.flam) end,
    function() params:add_number(L.id_prefix.."_flam_count", "flam hits (extra)", 0, 3, L.flam_count) end,
    function() params:add_number(L.id_prefix.."_flam_space", "flam spacing (steps)", 1, 4, L.flam_space) end,
    function() params:add_number(L.id_prefix.."_flam_vel", "flam vel delta", -30, 0, L.flam_vel) end,

    div(L.label.." · Velocity & Gate"),
    function()
      params:add_number(L.id_prefix.."_vel_fixed", "fixed velocity", 1, 127, L.fixed_velocity)
      params:set_action(L.id_prefix.."_vel_fixed", function(v) L.fixed_velocity = util.clamp(v,1,127) end)
    end,
    function()
      params:add_option(L.id_prefix.."_gate_mode", "MIDI gate", {"release","25%","50%","75%","100%"}, L.gate_mode)
      params:set_action(L.id_prefix.."_gate_mode", function(i) L.gate_mode = i end)
    end,

    div(L.label.." · Levels"),
    function()
      params:add_number(L.id_prefix.."_mx_vol_pct", "free mx volume (%)", 0, 200, L.mx_vol_pct)
      params:set_action(L.id_prefix.."_mx_vol_pct", function(v) L.mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,

    div(L.label.." · Transpose"),
    function()
      params:add_number(L.id_prefix.."_transpose_oct", "transpose (oct)", -4, 4, L.transpose_oct)
      params:set_action(L.id_prefix.."_transpose_oct", function(v) panic_all_outputs(); L.transpose_oct = util.clamp(v, -4, 4) end)
    end,
  })
end

local function add_arp_section()
  build_group("CHORDER · Arpeggio", {
    div("ARP · Toggle & Trigger"),
    function()
      params:add_option("arp_enable", "arpeggio", {"off","on"}, 1)
      params:set_action("arp_enable", function(i) if i==2 then Arp.start() else Arp.stop() end end)
    end,
    function() params:add_option("arp_trigger_mode", "trigger mode", {"key-held","latch"}, 1) end,
    function() params:add_option("arp_retrack", "retrack at cycle", {"off","on"}, 1) end,
    function() params:add_option("arp_free_run", "free-run (ignore key-held)", {"off","on"}, 1) end,

    div("ARP · Output"),
    function()
      params:add_option("arp_out_mode", "output", {"mx.samples","mx.samples + MIDI","MIDI"}, 1)
      params:set_action("arp_out_mode", function(_) Arp.hotplug_refresh() end)
    end,
    function()
      params:add_option("arp_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action("arp_mx_voice", function(i) MX.ensure_loaded(i) end)
    end,
    function()
      params:add_option("arp_midi_out_dev", "MIDI out", S.midi.devices, 1)
      params:set_action("arp_midi_out_dev", function(i) Arp.setup_midi_out(i) end)
    end,
    function()
      params:add_option("arp_midi_out_ch", "MIDI out ch", (function() local t={} for i=1,16 do t[i]=tostring(i) end; return t end)(), 1)
    end,
    function()
      params:add_number("arp_mx_vol_pct", "arp mx volume (%)", 0, 200, S.arp_mx_vol_pct)
      params:set_action("arp_mx_vol_pct", function(v) S.arp_mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,

    div("ARP · Timing"),
    function()
      ensure_quant_div_tables()
      local def_idx = 7
      safe_add_option("arp_div", "division", K.QUANT_DIV_OPTS, def_idx)
    end,
    function() params:add_option("arp_swing_mode", "swing mode", {"grid","swing %"}, 1) end,
    function() params:add_number("arp_swing_pct", "swing %", 0, 75, 0) end,

    div("ARP · Material"),
    function() params:add_option("arp_material", "material", {"chord tones","chord + passing"}, 2) end,

    div("ARP · Pattern Library"),
    function()
      local genres = (function() local t={} for _,g in ipairs(PatternLib.GENRES) do t[#t+1]=g end; table.sort(t); return t end)()
      local def = 1
      for i,n in ipairs(genres) do if n=="Basics" then def=i break end end
      params:add_option("arp_pat_genre", "genre", genres, def)
      params:set_action("arp_pat_genre", function(i)
        Arp.set_genre(i)
        local p = params:lookup_param("arp_pat_name")
        if p then
          local cur_genre = (params:lookup_param("arp_pat_genre").options or genres)[i] or "Basics"
          p.options = PatternLib.NAMES_FOR(cur_genre)
          p.count   = #p.options
          params:set("arp_pat_name", 1)
          Arp.set_pat(1)
        end
        update_arp_basics_visibility()
      end)
    end,
    function()
      local function current_genre()
        local pg = params:lookup_param("arp_pat_genre")
        return (pg and pg.options and pg.options[params:get("arp_pat_genre")]) or "Basics"
      end
      local names = PatternLib.NAMES_FOR(current_genre())
      params:add_option("arp_pat_name", "pattern", names, 1)
      params:set_action("arp_pat_name", function(i) Arp.set_pat(i) end)
    end,
    function()
      params:add_option("arp_pat_random_on_chord", "new random each chord", {"off","on"}, 1)
      params:set_action("arp_pat_random_on_chord", function(i) Arp.set_rand(i) end)
    end,

    div("ARP · Pitch/Range (Basics only)"),
    function() params:add_number("arp_transpose_oct", "transpose (oct)", -4, 4, 0) end,
    function() params:add_number("arp_octaves", "octave span (0-4)", 0, 4, 1) end,
    function() params:add_option("arp_oct_walk", "octave travel", {"wrap","bounce"}, 1) end,
    
    div("ARP · Order (Basics only)"),
    function()
      params:add_option("arp_order", "order", K.STRUM_OPTS, 1)
      params:set_action("arp_order", function(i) S.arp_order_idx = i end)
    end,

    div("ARP · Feel / Humanize"),
    function() params:add_number("arp_vel", "base velocity", 1, 127, 100) end,
    function() params:add_number("arp_vel_ramp", "velocity ramp/step", -24, 24, 0) end,
    function() params:add_number("arp_hum_steps", "humanize timing (max steps)", 0, 4, 0) end,
    function() params:add_number("arp_hum_vel", "humanize velocity (+/-)", 0, 30, 0) end,
    function() params:add_option("arp_gate", "gate", {"release","25%","50%","75%","100%"}, 1) end,
    function() params:add_number("arp_step_prob", "global step probability %", 0, 100, 100) end,
    function() params:add_number("arp_step_ratchet", "global ratchet x", 1, 8, 1) end,
    function() params:add_number("arp_ratchet_prob", "ratchet hit %", 0, 100, 100) end,
  })
end

-- ===== init =====
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

  add_io_section(default_midi_in_index)
  add_setup_section()
  add_timing_section()

  -- Free A section (defaults: IN ch=2, OUT ch=1)
  add_free_lane_section(S.freeA, { in_ch = 2, out_ch = 1 })
  -- Free B section (defaults: IN ch=3, OUT ch=1)
  add_free_lane_section(S.freeB, { in_ch = 3, out_ch = 1 })

  add_arp_section()

  midi.add = function(dev)
    print("MIDI added: "..(dev.name or "?"))
    rebuild_midi_lists()
    Free.setup_midi_out(S.freeA, params:get(S.freeA.midi_out_dev_param) or 1)
    Free.setup_midi_out(S.freeB, params:get(S.freeB.midi_out_dev_param) or 1)
    Arp.hotplug_refresh()
  end
  midi.remove = function(dev)
    print("MIDI removed: "..(dev.name or "?"))
    rebuild_midi_lists()
    setup_midi_in(params:get(S.midi.in_dev_param) or default_midi_in_index)
    MIDI.setup_out(params:get(S.midi.out_dev_param) or 1)
    Free.setup_midi_out(S.freeA, params:get(S.freeA.midi_out_dev_param) or 1)
    Free.setup_midi_out(S.freeB, params:get(S.freeB.midi_out_dev_param) or 1)
    Arp.hotplug_refresh()
  end

  show_param("chorder_swing_pct", S.swing_mode == 2)
  show_param("chorder_vel_fixed", S.velocity_mode == 1)
  show_param("chorder_inversion", S.voicing ~= 11)
  show_param(S.midi.gate_param, want_midi())

  -- visibility for both lanes
  for _,L in ipairs({S.freeA, S.freeB}) do
    show_param(L.id_prefix.."_vel_fixed", L.velocity_mode == 1)
    show_param(L.id_prefix.."_gate_mode", true)
    update_free_visibility(L)
  end
  update_arp_basics_visibility()

  recompute_root_midi()
  clock.run(clock_loop)

  MX.ensure_loaded(S.voice_index)
  MIDI.setup_out(1)
  Free.setup_midi_out(S.freeA, params:get(S.freeA.midi_out_dev_param) or 1)
  Free.setup_midi_out(S.freeB, params:get(S.freeB.midi_out_dev_param) or 1)
  Arp.hotplug_refresh()
  setup_midi_in(params:get(S.midi.in_dev_param) or default_midi_in_index)
  rebind_midi_in_if_needed()
  redraw()
end

-- ===== input handlers =====
function key(n, z)
  if n == 2 and z == 1 then
    S.page = (S.page % 5) + 1
    redraw()
  elseif n == 3 and z == 1 then
    -- K3: panic
    panic_all_outputs()
  end
end

function enc(n, d)
  if n == 1 then
    S.page = util.clamp(S.page + (d>0 and 1 or (d<0 and -1 or 0)), 1, 5)
    redraw()
    return
  end

  local items, cur_ref
  if S.page == S.PAGE_MAIN_IO then
    items, cur_ref = items_main_io(), "MAIN"
  elseif S.page == S.PAGE_FREEA_IO then
    items, cur_ref = items_free_io(S.freeA), "FREEA"
  elseif S.page == S.PAGE_FREEB_IO then
    items, cur_ref = items_free_io(S.freeB), "FREEB"
  elseif S.page == S.PAGE_ARP then
    items, cur_ref = items_arp_io(), "ARP"
  else
    items, cur_ref = items_chord_page(), "CHORD"
  end

  if n == 2 then
    local cur = clamp_cursor(S.cursor[cur_ref], #items)
    cur = util.clamp(cur + (d>0 and 1 or -1), 1, #items)
    S.cursor[cur_ref] = cur
    redraw()
    return
  end

  if n == 3 then
    local cur = clamp_cursor(S.cursor[cur_ref], #items)
    local it = items[cur]
    if it and it.delta then it.delta(d) end
    redraw()
    return
  end
end

function cleanup()
  if S.midi.input then S.midi.input.event = nil end
  panic_all_outputs()
end
