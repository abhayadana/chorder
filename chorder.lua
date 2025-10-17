-- CHORDER — a three-voice chord-based instrument
-- Author: @abhayadana (refactor pass by ChatGPT)
-- Engine: mx.samples and/or MIDI
--
-- Long press K3 for HUD

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
  -- UI pages
  PAGE_OUTPUT = 1, PAGE_CHORD = 2, PAGE_HUD = 3, page = 1,

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

  -- display / hud
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

  -- Free Play
  free = {
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
    -- chord build / voicing (Free)
    chord_type = 1, sus_mode = 1, inversion = 0, spread = 0, voicing = 1, add_bass = 1,
    -- dynamics (Free)
    ramp_per_step = 0, accent = 1, accent_amt = 0,
    -- humanize (Free)
    hum_steps_max = 0, hum_vel_range = 0,
    -- flam (Free)
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
  -- added slow divisions 2/1, 3/1, 4/1
  QUANT_DIV_OPTS = {"1/1","2/1","3/1","4/1","1/2","1/3","1/4","1/6","1/8","1/12","1/16","1/24","1/32"},
  QUANT_DIV_MAP  = {
    ["4/1"]=4/1, ["3/1"]=3/1, ["2/1"]=2/1, ["1/1"]=1/1, ["1/2"]=1/2, ["1/3"]=1/3, ["1/4"]=1/4,
    ["1/6"]=1/6, ["1/8"]=1/8, ["1/12"]=1/12, ["1/16"]=1/16, ["1/24"]=1/24, ["1/32"]=1/32
  },
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
    on_free  = mx_on_safe_free,
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

  private = {}
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
-- Pattern step format:
-- { d=1..7, oct=-2..+2, rest=true|nil, prob=0..1, r=ratchets>=1 }
local PatternLib = (function()
  local function deg(d, oct, prob, r, rest) return { d=d, oct=oct or 0, prob=prob or 1.0, r=r or 1, rest=rest or false } end
  local function clone(tbl) local t={}; for k,v in pairs(tbl) do t[k]=v end; return t end
  local function up1235(name) return { name=name, steps={deg(1),deg(2),deg(3),deg(5)} } end
  local function updown135(name) return { name=name, steps={deg(1),deg(3),deg(5),deg(3)} } end
  local function bounce15(name) return { name=name, steps={deg(1),deg(5,0),deg(1),deg(5,1)} } end
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
      { name="rock · riff 1-♭7-1", steps={deg(1,0),deg(7,-1),deg(1,0),deg(5,0)} },
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
    mout = nil, -- dedicated MIDI out (no sentinel)

    -- pattern system
    pat_enable = false,
    pat_genre = "Pop",
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

  local function build_order()
    local stype = params:get("arp_order") or 1
    return select(1, Strum.make(#A.notes_buf, stype, A.ord_state)) or {}
  end

  -- pattern helpers
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

  local function select_pattern()
    if not A.pat_enable then A.cur_pat=nil; return end
    local pat = (A.pat_random_on_chord and PatternLib.RAND(A.pat_genre)) or PatternLib.GET(A.pat_genre, A.pat_index)
    A.cur_pat = pat
    if pat then print("ARP pattern: "..(pat.name or "(unnamed)")) end
  end

  local function step_seconds()
    local opt = K.QUANT_DIV_OPTS[params:get("arp_div") or 9] or "1/8"
    local div = K.QUANT_DIV_MAP[opt] or 1/8
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
    if not A.running or not A.cur_pat or not S.last_voiced_notes then return end
    local steps = A.cur_pat.steps or {}
    if #steps==0 then return end
    local idx = ((A.step_i-1) % #steps) + 1
    local st = steps[idx] or {}
    local wait = step_seconds()

    -- combined step probability (pattern step prob * global arp_step_prob)
    local gprob = (params:get("arp_step_prob") or 100) / 100
    local prob = (st.prob or 1.0) * gprob
    local is_rest = st.rest or (math.random() > prob)

    if not is_rest and st.d then
      local n = degree_to_midi(util.clamp(st.d,1,7), st.oct or 0)
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

        -- ratchet handling: multiply pattern r by global ratchet
        local rpat = st.r or 1
        local rglob = params:get("arp_step_ratchet") or 1
        local rat_total = math.max(1, math.floor(rpat * rglob))
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
    end
    A.step_i = A.step_i + 1
  end

  local function next_note_legacy(step_idx)
    local ord = build_order(); if #ord==0 then return nil end
    local idx = ord[((step_idx-1) % #ord) + 1]
    local base = A.notes_buf[idx]
    local span = params:get("arp_octaves") or 1
    local walk = params:get("arp_oct_walk") or 1 -- 1=wrap, 2=bounce
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
    local tr = (params:get("arp_transpose_oct") or 0) * 12
    return util.clamp(base + o*12 + tr, 0, 127)
  end

  local function step_once_legacy()
    all_off()
    if not A.running then return end
    local wait = step_seconds()

    -- global step probability
    local gprob = (params:get("arp_step_prob") or 100) / 100
    if math.random() > gprob then
      A.step_i = A.step_i + 1
      return
    end

    local n = next_note_legacy(A.step_i)
    if n then
      local gm = params:get("arp_gate") or 1
      local frac = (gm==2 and 0.25) or (gm==3 and 0.50) or (gm==4 and 0.75) or 1.0
      local hsteps = params:get("arp_hum_steps") or 0
      local hvel   = params:get("arp_hum_vel") or 0
      local pre = 0
      if hsteps > 0 then
        local sub = math.random(0, hsteps)
        pre = (sub / 8) * wait
      end
      local vel_base = util.clamp((params:get("arp_vel") or 100) + (A.step_i-1)*(params:get("arp_vel_ramp") or 0), 1, 127)
      local vel2 = util.clamp(vel_base + math.random(-hvel, hvel), 1, 127)

      local rat_total = math.max(1, math.floor(params:get("arp_step_ratchet") or 1))
      local rat_prob  = (params:get("arp_ratchet_prob") or 100) / 100
      local sub_wait  = wait / rat_total
      local sub_dur   = sub_wait * frac

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

  local function run()
    A.running = true
    A.step_i  = 1
    if A.pat_enable then
      while A.running do
        local wait = step_seconds()
        if A.has_chord then
          if not A.cur_pat then select_pattern() end
          step_once_pattern()
        end
        clock.sleep(wait)
      end
    else
      make_material()
      while A.running do
        local wait = step_seconds()
        step_once_legacy()
        clock.sleep(wait)
        if params:get("arp_retrack")==2 and ((A.step_i-1) % math.max(#A.notes_buf,1) == 0) then
          make_material()
        end
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
    all_off()
  end

  local function chord_key(down)
    if down then
      A.has_chord = true
      if A.pat_enable then select_pattern() end
      A.step_i = 1
      start()
    else
      local mode = params:get("arp_trigger_mode") or 1 -- 1=key-held, 2=latch
      if mode==1 then stop() end
    end
  end

  local function hotplug_refresh()
    local sel = params:get("arp_midi_out_dev") or 1
    setup_midi_out(sel)
  end

  local function set_enable(v) A.pat_enable = (v==2); if A.pat_enable then select_pattern() end end
  local function set_genre_idx(i) local g = PatternLib.GENRES[i] or "Pop"; A.pat_genre = g; A.cur_pat=nil; select_pattern() end
  local function set_pat_idx(i) A.pat_index = i or 1; A.cur_pat=nil; select_pattern() end
  private = {}
  local function set_rand(v) A.pat_random_on_chord = (v==2) end

  return {
    start=start, stop=stop, refresh=make_material, chord_key=chord_key,
    setup_midi_out=setup_midi_out, hotplug_refresh=hotplug_refresh,
    set_pat_enable=set_enable, set_genre=set_genre_idx, set_pat=set_pat_idx, set_rand=set_rand
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

-- ===== Free module =====
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
    if S.free.mout then
      print("FREE MIDI OUT: connected to "..(midi.vports[real_port].name or ("port "..real_port)))
    else
      print("FREE MIDI OUT: connect failed for port "..real_port)
    end
  end

  local function free_note_on_mx(canon, note, vel) MX.on_free(canon, note, free_scaled_vel(vel)) end
  local function free_note_off_mx(canon, note) MX.off(canon, note) end

  local function free_note_on_midi(note, vel)
    if S.free.mout then
      pcall(function() S.free.mout:note_on(util.clamp(note,0,127), util.clamp(vel or 100,1,127), S.free.midi_channel) end)
    end
  end
  local function free_note_off_midi(note)
    if S.free.mout then
      pcall(function() S.free.mout:note_off(util.clamp(note,0,127), 0, S.free.midi_channel) end)
    end
  end

  local function free_fanout_on(canon, note, vel)
    if free_want_mx()   then free_note_on_mx(canon, note, vel) end
    if free_want_midi() then free_note_on_midi(note, vel) end
  end
  local function free_fanout_off(canon, note)
    if free_want_mx()   then free_note_off_mx(canon, note) end
    if free_want_midi() then free_note_off_midi(note) end
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

  local function quantize_to_scale(note)
    local sc = chordlib.build_scale(S.root_midi, S.scale_name)
    return chordlib.quantize_to_scale(sc, note)
  end

  local function free_make_strum_order(count)
    local tmp_state = { alt_flip=false, last_first=nil, last_last=nil }
    local ord = select(1, Strum.make(count, S.free.strum_type, tmp_state))
    return ord or {}
  end

  local function free_compute_step_offsets(m)
    local offs = Feel.step_offsets_one_index(
      m, S.free.strum_steps or 0, S.free.timing_shape or 1, S.free.timing_amt or 50, S.free.timing_skip_steps or 1
    )
    if (S.free.swing_mode or 1) == 2 and (S.free.swing_pct or 0) > 0 then
      local s = util.clamp(S.free.swing_pct, 0, 75) / 100
      local skew_steps = math.floor((S.free.strum_steps or 0) * s + 0.5)
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
    setup_midi_out = setup_free_midi_out_awake,
    fanout_on  = free_fanout_on,
    fanout_off = free_fanout_off,
    gate_for   = free_schedule_gate_for_note,
    act_names  = free_active_names,
    want_mx    = free_want_mx,
    want_midi  = free_want_midi,
    quantize_to_scale = quantize_to_scale,
    make_order = free_make_strum_order,
    step_offs  = free_compute_step_offsets,
  }
end)()

-- ===== Event queue / clock =====
local function queue_in_steps(steps, fn) table.insert(S.evq, {steps=math.max(0, steps), fn=fn}) end
local function schedule(step_steps, fn) if not S.quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end
local function free_schedule(step_steps, fn) if not S.free.quantize and step_steps == 0 then fn() else queue_in_steps(step_steps, fn) end end

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
  local notes, qual, name_root_midi = chordlib.build_chord{
    scale = sc,
    base_midi = root_note,
    degree = deg,
    chord_type = S.chord_type,
    inversion = S.inversion,
    spread = S.spread,
    sus_mode = S.sus_mode,
    voicing_mode = S.voicing,
    add_bass = S.add_bass,
    last_voiced_notes = S.last_voiced_notes,
    last_bass_note = S.last_bass_note
  }
  local symbol = chordlib.symbol_for(notes, qual, name_root_midi, {
    chord_type = S.chord_type, sus_mode = S.sus_mode, inversion = S.inversion,
    voicing_mode = S.voicing, add_bass = S.add_bass, spread = S.spread
  })
  return notes, symbol, qual, name_root_midi
end

local function handle_note_on(canon, root_note, vel, deg)
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

  local order = make_strum_order(#chord)
  local offs = Feel.step_offsets_zero_index(
    #order, S.strum_steps or 0, params:get("chorder_timing_shape") or 1, params:get("chorder_timing_amt") or 50,
    params:get("chorder_timing_skip_steps") or 1
  )
  local sorted = (function(t) local c={}; for i,n in ipairs(t) do c[i]=n end; table.sort(c); return c end)(chord)
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
    local s = (offs[k-1] or 0) + math.random(0, S.humanize_steps_max or 0)
    local v_base = util.clamp((vel or 100) + math.random(-(S.humanize_vel_range or 0),(S.humanize_vel_range or 0)), 1, 127)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = Feel.apply_velocity(
      k, #order, v_base, pos_in_sorted, n_sorted,
      params:get("chorder_ramp_per_step") or 0,
      params:get("chorder_accent") or 1,
      params:get("chorder_accent_amt") or 0
    )
    schedule(s, function()
      if (not hold_on) or (S.chord_hold_tokens[root_note] == tid) then fanout_note_on(canon, n, v) end
    end)
    if flam_on and flam_cnt > 0 then
      for j=1, flam_cnt do
        local ds = s + j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        schedule(ds, function()
          if (not hold_on) or (S.chord_hold_tokens[root_note] == tid) then fanout_note_on(canon, n, vv) end
        end)
      end
    end
  end
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

-- ===== Free handlers =====
local function free_handle_note_on(in_note, in_vel)
  local incoming = util.clamp(in_vel or 100, 1, 127)
  local base_play_vel = (S.free.velocity_mode == 1) and S.free.fixed_velocity or incoming
  local fcanon = S.canonical_names[S.free.voice_index] or ""

  if S.free.key_mode == 3 then
    local base = util.clamp(in_note + 12 * (S.free.transpose_oct or 0), 0, 127)
    local v = util.clamp(base_play_vel + math.random(-(S.free.hum_vel_range or 0),(S.free.hum_vel_range or 0)), 1, 127)
    S.free.active_map[in_note] = base
    Free.fanout_on(fcanon, base, v); Free.gate_for(base)
    return
  end

  local sc = scale128()
  local deg, base
  if S.free.key_mode == 1 then
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
  base = util.clamp(base + 12 * (S.free.transpose_oct or 0), 0, 127)

  if S.free.mode == 1 then
    local v = util.clamp(base_play_vel + math.random(-(S.free.hum_vel_range or 0),(S.free.hum_vel_range or 0)), 1, 127)
    S.free.active_map[in_note] = base
    Free.fanout_on(fcanon, base, v); Free.gate_for(base)
    return
  end

  local save_voicing, save_add = S.voicing, S.add_bass
  S.voicing, S.add_bass = S.free.voicing, (S.free.add_bass == 2)
  local notes, qual, name_root_midi = chordlib.build_chord{
    scale=sc, base_midi=base, degree=deg,
    chord_type=S.free.chord_type, inversion=S.free.inversion, spread=S.free.spread,
    sus_mode=S.free.sus_mode, voicing_mode=S.free.voicing, add_bass=(S.free.add_bass == 2),
    last_voiced_notes=nil, last_bass_note=nil
  }
  S.voicing, S.add_bass = save_voicing, save_add

  table.sort(notes)
  local ord = (#notes > 1 and S.free.strum_steps > 0) and Free.make_order(#notes) or (function(n) local t={} for i=1,n do t[i]=i end; return t end)(#notes)
  local offs = Free.step_offs(#ord)
  local sorted = (function(t) local c={}; for i,n in ipairs(t) do c[i]=n end; table.sort(c); return c end)(notes)
  local idx_in_sorted = {}; for i,n in ipairs(sorted) do if idx_in_sorted[n]==nil then idx_in_sorted[n]=i end end
  local n_sorted = #sorted

  local hold_on = (params:get("free_hold_strum") or 1) == 2
  local tid = next_trigger_id()
  if hold_on then S.free.hold_tokens[in_note] = tid end

  local emitted = {}
  for k, idx in ipairs(ord) do
    local n = notes[idx]
    local s = (offs[k] or 0) + math.random(0, S.free.hum_steps_max or 0)
    local v_base = util.clamp(base_play_vel + math.random(-(S.free.hum_vel_range or 0), (S.free.hum_vel_range or 0)), 1, 127)
    local pos_in_sorted = idx_in_sorted[n] or 1
    local v = Feel.apply_velocity(
      k, #ord, v_base, pos_in_sorted, n_sorted,
      params:get("free_ramp_per_step") or 0,
      params:get("free_accent") or 1,
      params:get("free_accent_amt") or 0
    )
    free_schedule(s, function()
      if (not hold_on) or (S.free.hold_tokens[in_note] == tid) then Free.fanout_on(fcanon, n, v); Free.gate_for(n) end
    end)
    if (params:get("free_flam") or S.free.flam) == 2 then
      local flam_cnt   = params:get("free_flam_count") or S.free.flam_count or 0
      local flam_space = params:get("free_flam_space") or S.free.flam_space or 1
      local flam_dvel  = params:get("free_flam_vel") or S.free.flam_vel or -8
      for j=1, flam_cnt do
        local ds = s + j * flam_space
        local vv = util.clamp(v + j * flam_dvel, 1, 127)
        free_schedule(ds, function()
          if (not hold_on) or (S.free.hold_tokens[in_note] == tid) then Free.fanout_on(fcanon, n, vv); Free.gate_for(n) end
        end)
      end
    end
    emitted[#emitted + 1] = n
  end
  S.free.chord_active[in_note] = emitted

  local symbol = chordlib.symbol_for(notes, qual, name_root_midi, {
    chord_type=S.free.chord_type, sus_mode=S.free.sus_mode, inversion=S.free.inversion,
    voicing_mode=S.free.voicing, add_bass=(S.free.add_bass == 2), spread=S.free.spread
  })
  S.free.last_name = symbol
  S.free.last_time = util.time()
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
  if (params:get("free_hold_strum") or 1) == 2 then S.free.hold_tokens[in_note] = nil end
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

  local parts = {}
  for _,q in pairs(S.free.active_map) do parts[#parts+1]=q end
  table.sort(parts)
  local fp = nil
  if #parts>0 then
    local names = {}
    for _,n in ipairs(parts) do local nm, oc = midi_to_name_oct(n); names[#names+1] = string.format("%s%d", nm, oc) end
    fp = table.concat(names, " ")
  end
  screen.level(fp and 12 or 10); screen.move(64, 50); screen.text_center("Free: " .. (fp or "—"))

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

  local t = {}
  for _,q in pairs(S.free.active_map) do t[#t+1] = q end
  table.sort(t)
  local fp = nil
  if #t>0 then
    local parts = {}
    for _,n in ipairs(t) do local nm, oc = midi_to_name_oct(n); parts[#parts+1] = string.format("%s%d", nm, oc) end
    fp = table.concat(parts, " ")
  end
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

-- ===== group-builder (2-pass param counting) =====
local _sep_seen = {}
local function div(label)
  return function() params:add_separator(("— %s —"):format(label or "")) end
end

local function build_group(title, builders)
  local real = params
  local counter = 0
  local proxy = {}
  local function bump() counter = counter + 1 end
  -- proxy implements add_* and ignores side effects during probe
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

  -- pass 1: count
  params = proxy
  for _,fn in ipairs(builders) do fn() end
  params = real

  -- create group with exact count
  params:add_group(title, counter)

  -- pass 2: real adds
  for _,fn in ipairs(builders) do fn() end
end

-- ===== MIDI In setup =====
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

    local chord_ok
    do
      local ch_sel_idx = params:get(S.midi.in_ch_param)
      chord_ok = (ch_sel_idx == 1) or (msg.ch == (ch_sel_idx - 1))
    end

    local free_ok
    do
      local free_ch = params:get(S.free.midi_in_ch_param) or 2
      free_ok = (msg.ch == free_ch)
    end

    local canon = S.canonical_names[S.voice_index] or ""
    local chord_vel = (S.velocity_mode == 2 and (msg.vel or S.fixed_velocity) or S.fixed_velocity)

    if msg.type == "note_on" and msg.vel > 0 then
      if chord_ok then
        MX.ensure_loaded(S.voice_index)
        local pc = msg.note % 12
        if K.WHITE_SET[pc] then
          local deg = K.WHITE_TO_DEG[pc]
          if deg then handle_note_on(canon, msg.note, chord_vel, deg) end
        end
      end

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
local function show_param(id, show) if show then params:show(id) else params:hide(id) end end

local function update_free_visibility()
  local chroma = (S.free.key_mode == 3)
  show_param("free_mode",                not chroma)
  show_param("free_chord_type",          not chroma)
  show_param("free_sus_mode",            not chroma)
  show_param("free_inversion",           not chroma)
  show_param("free_spread",              not chroma)
  show_param("free_voicing",             not chroma)
  show_param("free_add_bass",            not chroma)
  show_param("free_strum_steps",         not chroma)
  show_param("free_strum_type",          not chromma)
  show_param("free_timing_shape",        not chroma)
  show_param("free_timing_amt",          not chroma)
  show_param("free_timing_skip_steps",   not chroma)
  show_param("free_hum_steps",           not chroma)
  show_param("free_hum_vel",             not chroma)
  show_param("free_ramp_per_step",       not chroma)
  show_param("free_accent",              not chroma)
  show_param("free_accent_amt",          not chroma)
  show_param("free_flam",                not chroma)
  show_param("free_flam_count",          not chroma)
  show_param("free_flam_space",          not chroma)
  show_param("free_flam_vel",            not chroma)
  show_param("free_swing_mode",          not chroma)
  show_param("free_swing_pct",           not chroma)
  show_param("free_quantize",            not chroma)
end

-- ===== sections (using build_group) =====
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
      params:add_option(S.midi.in_ch_param, "chord MIDI input ch", ch_opts_in, 1)
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
        local pf = params:lookup_param("free_mx_voice")
        if pf then
          pf.options = (#S.display_names>0 and S.display_names or {"(no packs)"})
          pf.count   = (#S.display_names>0 and #S.display_names or 1)
          S.free.voice_index = 1; params:set("free_mx_voice", S.free.voice_index)
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

    div("Timing · Gate (Chord MIDI)"),
    function() params:add_option(S.midi.gate_param, "MIDI gate", S.midi.gate_opts, 1) end,

    div("Timing · Strum"),
    function()
      params:add_number("chorder_strum", "strum (steps of division)", 0, 8, S.strum_steps)
      params:set_action("chorder_strum", function(v) S.strum_steps = util.clamp(v,0,8); redraw() end)
    end,
    function()
      params:add_option("chorder_strum_type", "strum type", K.STRUM_OPTS, S.strum_type)
      params:set_action("chorder_strum_type", function(i) S.strum_type = i; strum_state = { alt_flip=false, last_first=nil, last_last=nil }; redraw() end)
    end,
    function() params:add_option("chorder_hold_strum", "hold-to-strum (Chord)", {"off","on"}, 1) end,

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

local function add_free_section()
  build_group("CHORDER · Free Play", {
    div("Free · Toggle"),
    function()
      params:add_option("free_enable", "free play", {"off","on"}, 1)
      params:set_action("free_enable", function(i) S.free.enable = (i==2); if not S.free.enable then panic_all_outputs() end; redraw() end)
    end,

    div("Free · MIDI In"),
    function() params:add_option(S.free.midi_in_ch_param, "MIDI input ch", (function() local t={} for i=1,16 do t[#t+1]=tostring(i) end; return t end)(), 2) end,

    div("Free · Key Input Mode"),
    function()
      params:add_option(S.free.key_mode_param, "",
        {"white keys (diatonic degrees)","all keys → quantized to scale","all keys chromatic (mono only)"}, S.free.key_mode)
      params:set_action(S.free.key_mode_param, function(i)
        S.free.key_mode = i
        if S.free.key_mode == 3 then
          params:set("free_mode", 1); S.free.mode = 1; panic_all_outputs()
        end
        update_free_visibility(); redraw()
      end)
    end,

    div("Free · Instrument & Output"),
    function()
      params:add_option(S.free.out_mode_param, "output", S.free.out_opts, 1)
      params:set_action(S.free.out_mode_param, function(_) panic_all_outputs(); rebind_midi_in_if_needed(); show_param("free_gate_mode", true); redraw() end)
    end,
    function()
      params:add_option("free_mx_voice", "mx.samples", (#S.display_names>0 and S.display_names or {"(no packs)"}), 1)
      params:set_action("free_mx_voice", function(i) S.free.voice_index = i; MX.ensure_loaded(i); redraw() end)
    end,

    div("Free · MIDI Out"),
    function()
      params:add_option(S.free.midi_out_dev_param, "MIDI out", S.midi.devices, 1)
      params:set_action(S.free.midi_out_dev_param, function(i) Free.setup_midi_out(i); panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,
    function()
      params:add_option(S.free.midi_out_ch_param, "MIDI out ch", (function() local t={} for i=1,16 do t[i]=tostring(i) end; return t end)(), 1)
      params:set_action(S.free.midi_out_ch_param, function(idx) S.free.midi_channel = idx; panic_all_outputs(); rebind_midi_in_if_needed(); redraw() end)
    end,

    div("Free · Mode"),
    function()
      params:add_option("free_mode", "play mode", {"mono","chord"}, S.free.mode)
      params:set_action("free_mode", function(i) S.free.mode = i end)
    end,

    div("Free · Chord Build"),
    function() params:add_option("free_chord_type", "chord type", {"triad","7th","9th"}, S.free.chord_type); params:set_action("free_chord_type", function(i) S.free.chord_type = i end) end,
    function() params:add_option("free_sus_mode", "third handling", {"normal","sus2","sus4"}, S.free.sus_mode); params:set_action("free_sus_mode", function(i) S.free.sus_mode = i end) end,
    function() params:add_number("free_inversion", "inversion (0-3)", 0, 3, S.free.inversion); params:set_action("free_inversion", function(v) S.free.inversion = util.clamp(v,0,3) end) end,
    function() params:add_number("free_spread", "spread (semitones)", -24, 24, S.free.spread); params:set_action("free_spread", function(v) S.free.spread = util.round(v) end) end,
    function()
      params:add_option("free_voicing", "voicing",
        {"none","drop-2","drop-3","drop-2&4","drop-1","open","wide","quartal","quintal","nearest","smooth"}, S.free.voicing)
      params:set_action("free_voicing", function(i) S.free.voicing = i end)
    end,
    function()
      params:add_option("free_add_bass", "add bass (root -12)", {"off","on"}, S.free.add_bass)
      params:set_action("free_add_bass", function(i) S.free.add_bass = i end)
    end,

    div("Free · Strum"),
    function() params:add_number("free_strum_steps", "strum (steps of division)", 0, 8, S.free.strum_steps); params:set_action("free_strum_steps", function(v) S.free.strum_steps = util.clamp(v,0,8) end) end,
    function() params:add_option("free_strum_type", "strum type", K.STRUM_OPTS, S.free.strum_type); params:set_action("free_strum_type", function(i) S.free.strum_type = i end) end,
    function() params:add_option("free_hold_strum", "hold-to-strum (Free)", {"off","on"}, 1) end,

    div("Free · Timing"),
    function() params:add_option("free_timing_shape", "timing shape", {"straight","serpentine","accelerando","ritardando","rake ease-in","rake ease-out","skip alt gaps"}, S.free.timing_shape); params:set_action("free_timing_shape", function(i) S.free.timing_shape = i end) end,
    function() params:add_number("free_timing_amt", "timing amount", 0, 100, S.free.timing_amt); params:set_action("free_timing_amt", function(v) S.free.timing_amt = util.clamp(v,0,100) end) end,
    function() params:add_number("free_timing_skip_steps", "skip size (steps)", 0, 8, S.free.timing_skip_steps); params:set_action("free_timing_skip_steps", function(v) S.free.timing_skip_steps = util.clamp(v,0,8) end) end,

    div("Free · Swing & Quantize"),
    function() params:add_option("free_swing_mode", "swing mode", {"grid","swing %"}, S.free.swing_mode) end,
    function() params:add_number("free_swing_pct", "swing %", 0, 75, S.free.swing_pct); params:set_action("free_swing_pct", function(v) S.free.swing_pct = util.clamp(v,0,75) end) end,
    function() params:add_option("free_quantize", "quantize free hits", {"off","on"}, (S.free.quantize and 2 or 1)); params:set_action("free_quantize", function(i) S.free.quantize = (i==2) end) end,

    div("Free · Humanize"),
    function() params:add_number("free_hum_steps", "humanize timing (max steps)", 0, 4, S.free.hum_steps_max); params:set_action("free_hum_steps", function(v) S.free.hum_steps_max = util.clamp(v,0,4) end) end,
    function() params:add_number("free_hum_vel", "humanize velocity (+/-)", 0, 30, S.free.hum_vel_range); params:set_action("free_hum_vel", function(v) S.free.hum_vel_range = util.clamp(v,0,30) end) end,

    div("Free · Dynamics"),
    function() params:add_number("free_ramp_per_step", "velocity ramp / step", -24, 24, S.free.ramp_per_step) end,
    function() params:add_option("free_accent", "accent target", {"none","bass","top","middle"}, S.free.accent) end,
    function() params:add_number("free_accent_amt", "accent amount", -30, 30, S.free.accent_amt) end,

    div("Free · Flam"),
    function() params:add_option("free_flam", "flam", {"off","on"}, S.free.flam) end,
    function() params:add_number("free_flam_count", "flam hits (extra)", 0, 3, S.free.flam_count) end,
    function() params:add_number("free_flam_space", "flam spacing (steps)", 1, 4, S.free.flam_space) end,
    function() params:add_number("free_flam_vel", "flam vel delta", -30, 0, S.free.flam_vel) end,

    div("Free · Velocity & Gate"),
    function()
      params:add_option("free_vel_mode", "velocity src (free)", {"fixed","incoming"}, S.free.velocity_mode)
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

    div("Free · Levels"),
    function()
      params:add_number("free_mx_vol_pct", "free mx volume (%)", 0, 200, S.free.mx_vol_pct)
      params:set_action("free_mx_vol_pct", function(v) S.free.mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,

    div("Free · Transpose"),
    function()
      params:add_number("free_transpose_oct", "free play transpose (oct)", -4, 4, S.free.transpose_oct)
      params:set_action("free_transpose_oct", function(v) panic_all_outputs(); S.free.transpose_oct = util.clamp(v, -4, 4) end)
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

    div("ARP · Timing"),
    function() params:add_option("arp_div", "division", K.QUANT_DIV_OPTS, 9) end, -- default 1/8
    function() params:add_option("arp_swing_mode", "swing mode", {"grid","swing %"}, 1) end,
    function() params:add_number("arp_swing_pct", "swing %", 0, 75, 0) end,

    div("ARP · Material"),
    function() params:add_option("arp_material", "material", {"chord tones","chord + passing"}, 2) end,

    div("ARP · Order & Range"),
    function() params:add_option("arp_order", "order", K.STRUM_OPTS, 1) end,
    function() params:add_number("arp_octaves", "octave span", 0, 3, 1) end,
    function() params:add_option("arp_oct_walk", "octave walk", {"wrap","bounce"}, 1) end,

    div("ARP · Patterns"),
        function()
          params:add_option("arp_pat_enable", "pattern mode", {"off","on"}, 1)
          params:set_action("arp_pat_enable", function(i) Arp.set_pat_enable(i) end)
        end,
        function()
          params:add_option("arp_pat_genre", "pattern genre", PatternLib.GENRES, 1)
          params:set_action("arp_pat_genre", function(i)
            local g = PatternLib.GENRES[i] or "Pop"
            local names = PatternLib.NAMES_FOR(g)
            local p = params:lookup_param("arp_pat_name")
            if p then p.options = names; p.count = #names; params:set("arp_pat_name", 1) end
            Arp.set_genre(i); Arp.set_pat(1)
          end)
        end,
        function()
          params:add_option("arp_pat_name", "pattern", PatternLib.NAMES_FOR(PatternLib.GENRES[1]), 1)
          params:set_action("arp_pat_name", function(i) Arp.set_pat(i) end)
        end,
        function()
          params:add_option("arp_pat_random", "random pattern on chord", {"off","on"}, 1)
          params:set_action("arp_pat_random", function(i) Arp.set_rand(i) end)
        end,

    div("ARP · Probability & Ratchet"),
    function() params:add_number("arp_step_prob", "step probability (%)", 0, 100, 100) end,
    function() params:add_number("arp_step_ratchet", "ratchet (x)", 1, 8, 1) end,
    function() params:add_number("arp_ratchet_prob", "ratchet probability (%)", 0, 100, 100) end,

    div("ARP · Transpose"),
    function() params:add_number("arp_transpose_oct", "transpose (oct)", -4, 4, 0) end,

    div("ARP · Humanize"),
    function() params:add_number("arp_hum_steps", "humanize timing (max steps)", 0, 4, 0) end,
    function() params:add_number("arp_hum_vel", "humanize velocity (+/-)", 0, 30, 0) end,

    div("ARP · Velocity & Gate"),
    function() params:add_number("arp_vel", "base velocity", 1, 127, 100) end,
    function() params:add_number("arp_vel_ramp", "vel ramp/step", -24, 24, 0) end,
    function() params:add_option("arp_gate", "gate", {"release","25%","50%","75%","100%"}, 3) end,

    div("ARP · Levels"),
    function()
      params:add_number("arp_mx_vol_pct", "arpeggio mx volume (%)", 0, 200, S.arp_mx_vol_pct)
      params:set_action("arp_mx_vol_pct", function(v) S.arp_mx_vol_pct = util.clamp(math.floor(v or 100), 0, 200) end)
    end,
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
  add_free_section()
  add_arp_section()

  midi.add = function(dev)
    print("MIDI added: "..(dev.name or "?"))
    rebuild_midi_lists()
    Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
    Arp.hotplug_refresh()
  end
  midi.remove = function(dev)
    print("MIDI removed: "..(dev.name or "?"))
    rebuild_midi_lists()
    setup_midi_in(params:get(S.midi.in_dev_param) or default_midi_in_index)
    MIDI.setup_out(params:get(S.midi.out_dev_param) or 1)
    Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
    Arp.hotplug_refresh()
  end

  show_param("chorder_swing_pct", S.swing_mode == 2)
  show_param("chorder_vel_fixed", S.velocity_mode == 1)
  show_param("chorder_inversion", S.voicing ~= 11)
  show_param(S.midi.gate_param, want_midi())
  show_param("free_vel_fixed", S.free.velocity_mode == 1)
  show_param("free_gate_mode", Free.want_midi())
  update_free_visibility()

  recompute_root_midi()
  clock.run(clock_loop)

  MX.ensure_loaded(S.voice_index)
  MIDI.setup_out(1)
  Free.setup_midi_out(params:get(S.free.midi_out_dev_param) or 1)
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
        panic_all_outputs()
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
