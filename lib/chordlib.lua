-- lib/chordlib.lua
-- Chord/voicing math extracted for CHORDER
-- Author: @abhayadana (library split by ChatGPT)
--
-- API:
--   local chordlib = include "lib/chordlib"
--   local sc = chordlib.build_scale(root_midi, scale_name)
--   local nidx = chordlib.nearest_index_with_degree(sc, midi, degree)
--   local nn, qual, name_root_midi = chordlib.build_chord{
--       scale=sc, base_midi=nn_base, degree=deg,
--       chord_type=1|2|3, inversion=0..3, spread=int, sus_mode=1|2|3,
--       voicing_mode=1..11, add_bass=bool,
--       last_voiced_notes=table|nil, last_bass_note=int|nil
--   }
--   local sym = chordlib.symbol_for(nn, qual, name_root_midi, {
--       chord_type=..., sus_mode=..., inversion=..., voicing_mode=...,
--       add_bass=..., spread=...
--   })
--   local qnote, qi = chordlib.quantize_to_scale(sc, note)

local musicutil = require "musicutil"

local M = {}

M.NOTE_NAMES_SHARP = {"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"}

-- ---------- helpers ----------
local function clone_sorted(t)
  if type(t)~="table" then return {} end
  local c = {}; for i,n in ipairs(t) do c[i]=n end
  table.sort(c); return c
end

local function triad_quality(notes)
  local v = clone_sorted(notes)
  if #v < 3 then return "unk" end
  local a = v[2]-v[1]
  local b = v[3]-v[2]
  if a==4 and b==3 then return "maj" end
  if a==3 and b==4 then return "min" end
  if a==3 and b==3 then return "dim" end
  if a==4 and b==4 then return "aug" end
  return "unk"
end

-- voicing transforms
local function apply_drop(v, which_from_top)
  local t = clone_sorted(v)
  if #t < which_from_top then return t end
  local idx = #t - which_from_top + 1
  t[idx] = t[idx] - 12
  table.sort(t); return t
end

local function apply_drop_multiple(v, list_from_top)
  local t = clone_sorted(v)
  table.sort(list_from_top)
  for i=#list_from_top,1,-1 do t = apply_drop(t, list_from_top[i]) end
  return t
end

local function apply_open_position(v)
  local t = clone_sorted(v)
  if #t == 3 then
    t = { t[1], t[3], t[2]+12 }
  elseif #t == 4 then
    t = { t[1], t[3], t[4], t[2]+12 }
  end
  table.sort(t); return t
end

local function apply_wide_spread(v)
  local t = clone_sorted(v)
  for i=2,#t,2 do t[i] = t[i] + 12 end
  table.sort(t); return t
end

local function diatonic_stack(sc, i_root, steps, count)
  local idxs = {}
  for k=0,(count-1) do idxs[#idxs+1] = i_root + k*steps end
  return idxs
end

local function assign_register_nearest(target, prev)
  local out = clone_sorted(target)
  if not prev or #prev==0 or #prev ~= #out then
    -- center around ~C4
    local center = 60
    local base = out[1]
    local shift = math.floor((center - base) / 12)
    for i=1,#out do out[i] = out[i] + shift*12 end
    table.sort(out); return out
  end
  local v = {}; for i=1,#out do v[i]=out[i] end
  local prev_low = prev[1]
  local base = v[1]
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

local function choose_smooth_inversion(base_idxs, sc, last_bass, want7)
  if not last_bass then return base_idxs end
  local best_idxs, best = base_idxs, nil
  local max_inv = math.max(0, (want7 and 3 or 2))
  for inv=0,max_inv do
    local idxs = {}
    for _,ix in ipairs(base_idxs) do idxs[#idxs+1] = ix end
    for _=1, math.min(inv, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
    local n0 = sc[math.max(1, math.min(idxs[1], #sc))]
    local d = math.abs(n0 - last_bass)
    if not best or d < best then best = d; best_idxs = idxs end
  end
  return best_idxs
end

-- ---------- public helpers ----------
function M.build_scale(root_midi, scale_name)
  return musicutil.generate_scale_of_length(root_midi, scale_name, 128)
end

function M.nearest_index_with_degree(sc, midi, target_deg)
  local best_i, best_d = 1, 999
  for i=1,#sc do
    if 1 + ((i-1) % 7) == target_deg then
      local d = math.abs(sc[i] - midi)
      if d < best_d then best_d, best_i = d, i end
    end
  end
  return best_i
end

function M.quantize_to_scale(sc, note)
  local best_i, best_d, best_val = 1, 1e9, sc[1]
  for i=1,#sc do
    local v = sc[i]; local d = math.abs(v - note)
    if d < best_d or (d == best_d and v > best_val) then best_d, best_i, best_val = d, i, v end
  end
  return best_val, best_i
end

-- options:
--   scale (table) *required*
--   base_midi, degree (1..7) *required*
--   chord_type: 1=triad, 2=7th, 3=9th
--   inversion: 0..3
--   spread: integer semitones
--   sus_mode: 1=normal, 2=sus2, 3=sus4
--   voicing_mode: 1..11 (see tags below)
--   add_bass: boolean
--   last_voiced_notes: table|nil (for voicing_mode=10)
--   last_bass_note: int|nil (for voicing_mode=11)
function M.build_chord(opts)
  local sc          = assert(opts.scale, "build_chord: opts.scale required")
  local base_midi   = assert(opts.base_midi, "build_chord: opts.base_midi required")
  local deg         = assert(opts.degree, "build_chord: opts.degree required")
  local chord_type  = opts.chord_type or 1
  local inv         = opts.inversion or 0
  local spread      = opts.spread or 0
  local sus_sel     = opts.sus_mode or 1
  local voicing     = opts.voicing_mode or 1
  local add_bass    = not not opts.add_bass
  local prev_notes  = opts.last_voiced_notes
  local last_bass   = opts.last_bass_note

  local i_root = M.nearest_index_with_degree(sc, base_midi, deg)

  local idxs = { i_root, i_root+2, i_root+4 }
  local want7 = (chord_type >= 2)
  local want9 = (chord_type >= 3)
  if want7 then idxs[#idxs+1] = i_root+6 end
  if want9 then idxs[#idxs+1] = i_root+8 end

  -- sus replaces the 3rd
  if sus_sel ~= 1 then
    for k = 1, #idxs do
      if idxs[k] == i_root+2 then
        if     sus_sel == 2 then idxs[k] = i_root+1 -- sus2
        elseif sus_sel == 3 then idxs[k] = i_root+3 -- sus4
        end
        break
      end
    end
  end

  -- inversion (skipped when using "smooth" voicing)
  if voicing ~= 11 then
    for _ = 1, math.min(inv, #idxs-1) do
      local n = table.remove(idxs, 1)
      table.insert(idxs, n + 7)
    end
  end

  local notes = {}
  for _,ix in ipairs(idxs) do
    local n = sc[math.max(1, math.min(ix, #sc))]
    notes[#notes+1] = n + spread
  end

  local triad3 = {notes[1], notes[2], notes[3]}
  local quality = triad_quality(triad3)
  local name_root_midi = sc[i_root]

  -- apply voicing mode
  if     voicing == 1 then
    -- none
  elseif voicing == 2 then notes = apply_drop(notes, 2)
  elseif voicing == 3 then notes = apply_drop(notes, 3)
  elseif voicing == 4 then notes = apply_drop_multiple(notes, {2,4})
  elseif voicing == 5 then notes = apply_drop(notes, 1)
  elseif voicing == 6 then notes = apply_open_position(notes)
  elseif voicing == 7 then notes = apply_wide_spread(notes)
  elseif voicing == 8 or voicing == 9 then
    local steps = (voicing == 8) and 3 or 4
    local count = want7 and 4 or 3
    local i0 = idxs[1]
    local vdx = diatonic_stack(sc, i0, steps, count)
    local out = {}; for _,ix in ipairs(vdx) do out[#out+1] = sc[math.max(1, math.min(ix, #sc))] end
    notes = out
  elseif voicing == 10 then
    notes = assign_register_nearest(notes, prev_notes)
  elseif voicing == 11 then
    local idxs2 = choose_smooth_inversion(idxs, sc, last_bass, want7)
    local out = {}; for _,ix in ipairs(idxs2) do out[#out+1] = sc[math.max(1, math.min(ix, #sc))] end
    notes = out
  end

  if add_bass then notes[#notes+1] = (notes[1] - 12) end
  table.sort(notes)
  return notes, quality, name_root_midi
end

local function base_chord_symbol(root_pc_val, qual, chord_type_sel, sus_sel)
  local root_txt = M.NOTE_NAMES_SHARP[(root_pc_val % 12) + 1]
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

-- opts: chord_type, sus_mode, inversion, voicing_mode, add_bass, spread
function M.symbol_for(notes, qual, name_root_midi, opts)
  opts = opts or {}
  if type(notes) ~= "table" or #notes == 0 then return "" end
  local root_pc_val = name_root_midi % 12
  local symbol = base_chord_symbol(root_pc_val, qual, opts.chord_type or 1, opts.sus_mode or 1)

  -- slash bass if different
  local bass = notes[1]; for i=2,#notes do if notes[i] < bass then bass = notes[i] end end
  local bass_pc = bass % 12
  if bass_pc ~= root_pc_val then symbol = symbol .. "/" .. M.NOTE_NAMES_SHARP[bass_pc + 1] end

  local tags = {}
  local vmode = opts.voicing_mode or 1
  if (opts.inversion or 0) > 0 and vmode ~= 11 then table.insert(tags, "inv"..tostring(opts.inversion or 0)) end
  if     vmode == 2 then table.insert(tags, "drop2")
  elseif vmode == 3 then table.insert(tags, "drop3")
  elseif vmode == 4 then table.insert(tags, "drop2&4")
  elseif vmode == 5 then table.insert(tags, "drop1")
  elseif vmode == 6 then table.insert(tags, "open")
  elseif vmode == 7 then table.insert(tags, "wide")
  elseif vmode == 8 then table.insert(tags, "quartal")
  elseif vmode == 9 then table.insert(tags, "quintal")
  elseif vmode == 10 then table.insert(tags, "nearest")
  elseif vmode == 11 then table.insert(tags, "smooth") end

  if opts.add_bass and bass_pc == root_pc_val then table.insert(tags, "bass") end
  local spread = opts.spread or 0
  if spread ~= 0 then
    local s = (spread > 0) and ("+"..spread) or tostring(spread)
    table.insert(tags, "spread"..s)
  end
  if #tags > 0 then symbol = symbol .. " (" .. table.concat(tags, ", ") .. ")" end
  return symbol
end

return M
