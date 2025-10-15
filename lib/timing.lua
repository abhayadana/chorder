-- lib/timing.lua
-- Timing helpers shared by CHORDER main script and any submodules (e.g., Strum/Arp/Free)
-- Author: @abhayadana (factored into a module by ChatGPT)

local Timing = {}

-- Internal helpers
local function clamp(v, lo, hi)
  if v < lo then return lo elseif v > hi then return hi else return v end
end

local function round(x)
  return util and util.round and util.round(x) or math.floor(x + 0.5)
end

-- Easing
local function ease_in_quad(t)  return t * t end
local function ease_out_quad(t) return 1 - (1 - t) * (1 - t) end

--[[--------------------------------------------------------------------------
step_offsets_zero_index(count, base_steps, shape, amt_percent, skip_steps)

Returns a table of per-note offsets in "grid steps" for a chord strum,
**using a zero-based first entry** so that offsets[0] == 0 (to match the
original CHORDER chord path usage).

- count        : number of notes in the strum order
- base_steps   : base spacing in steps (0..8 typically)
- shape        : 1=straight, 2=serpentine, 3=accelerando, 4=ritardando,
                 5=rake ease-in, 6=rake ease-out, 7=skip alt gaps
- amt_percent  : 0..100, intensity of the shape
- skip_steps   : used only for shape 7
---------------------------------------------------------------------------]]
function Timing.step_offsets_zero_index(count, base_steps, shape, amt_percent, skip_steps)
  local m = math.max(0, count or 0)
  local base = math.max(0, base_steps or 0)
  local amt  = clamp((amt_percent or 50) / 100.0, 0, 1)
  local offs = {}

  -- Degenerate cases
  if m <= 1 or base == 0 then
    offs[0] = 0
    for k = 1, m - 1 do offs[k] = 0 end
    return offs
  end

  local spacing = {}
  for k = 1, m - 1 do spacing[k] = base end

  local function apply_serpentine()
    local mid = (m + 1) / 2
    for k = 1, m - 1 do
      local d = math.abs((k - mid) / mid)
      spacing[k] = base * (1 + amt * d)
    end
  end

  local function apply_accel()
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      spacing[k] = math.max(0.1, base * (1 - amt * t))
    end
  end

  local function apply_rit()
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      spacing[k] = base * (1 + amt * t)
    end
  end

  local function apply_rake(ease_fn)
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      local w = ease_fn(t)
      spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5) * 2))
      spacing[k] = math.max(0.1, spacing[k])
    end
  end

  local function apply_skip_alt()
    local add = (skip_steps or 0) * amt
    for k = 1, m - 1 do
      if (k % 2) == 1 then spacing[k] = spacing[k] + add end
    end
  end

  if     shape == 2 then apply_serpentine()
  elseif shape == 3 then apply_accel()
  elseif shape == 4 then apply_rit()
  elseif shape == 5 then apply_rake(ease_in_quad)
  elseif shape == 6 then apply_rake(ease_out_quad)
  elseif shape == 7 then apply_skip_alt()
  end

  local sum = 0
  offs[0] = 0
  for k = 1, m - 1 do
    sum = sum + round(spacing[k])
    offs[k] = sum
  end
  return offs
end

--[[--------------------------------------------------------------------------
step_offsets_one_index(count, base_steps, shape, amt_percent, skip_steps)

Same as above, but returns **1-based offsets** so that offs[1] == 0,
offs[2] >= 0, ... (to match original Free path usage).
---------------------------------------------------------------------------]]
function Timing.step_offsets_one_index(count, base_steps, shape, amt_percent, skip_steps)
  local m = math.max(0, count or 0)
  local base = math.max(0, base_steps or 0)
  local amt  = clamp((amt_percent or 50) / 100.0, 0, 1)
  local offs = {}

  if m <= 1 or base == 0 then
    for k = 1, m do offs[k] = 0 end
    return offs
  end

  local spacing = {}
  for k = 1, m - 1 do spacing[k] = base end

  local function apply_serpentine()
    local mid = (m + 1) / 2
    for k = 1, m - 1 do
      local d = math.abs((k - mid) / mid)
      spacing[k] = base * (1 + amt * d)
    end
  end

  local function apply_accel()
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      spacing[k] = math.max(0.1, base * (1 - amt * t))
    end
  end

  local function apply_rit()
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      spacing[k] = base * (1 + amt * t)
    end
  end

  local function apply_rake(ease_fn)
    for k = 1, m - 1 do
      local t = (k - 1) / math.max(1, (m - 2))
      local w = ease_fn(t)
      spacing[k] = base * (1 - amt + amt * (1 + (w - 0.5) * 2))
      spacing[k] = math.max(0.1, spacing[k])
    end
  end

  local function apply_skip_alt()
    local add = (skip_steps or 0) * amt
    for k = 1, m - 1 do
      if (k % 2) == 1 then spacing[k] = spacing[k] + add end
    end
  end

  if     shape == 2 then apply_serpentine()
  elseif shape == 3 then apply_accel()
  elseif shape == 4 then apply_rit()
  elseif shape == 5 then apply_rake(ease_in_quad)
  elseif shape == 6 then apply_rake(ease_out_quad)
  elseif shape == 7 then apply_skip_alt()
  end

  local sum = 0
  for k = 1, m do
    if k == 1 then
      offs[k] = 0
    else
      sum = sum + round(spacing[k - 1])
      offs[k] = sum
    end
  end
  return offs
end

--[[--------------------------------------------------------------------------
apply_velocity(k, m, base_velocity, pos_in_sorted, n_sorted,
               ramp_per_step, accent_target, accent_amount)

Returns a clamped velocity for the k-th emitted note (1..m), given:
- base_velocity  : starting velocity (1..127)
- ramp_per_step  : delta added each step (e.g., -6)
- accent_target  : 1=none, 2=bass, 3=top, 4=middle
- accent_amount  : +/- amount to add on the target (e.g., +12)
- pos_in_sorted  : index of this note in the pitch-sorted set (1..n_sorted)
---------------------------------------------------------------------------]]
function Timing.apply_velocity(k, m, base_velocity, pos_in_sorted, n_sorted, ramp_per_step, accent_target, accent_amount)
  local v = base_velocity or 100
  v = v + (k - 1) * (ramp_per_step or 0)

  local acc_tgt = accent_target or 1
  local acc_amt = accent_amount or 0

  if acc_tgt == 2 then
    if pos_in_sorted == 1 then v = v + acc_amt end
  elseif acc_tgt == 3 then
    if pos_in_sorted == (n_sorted or 1) then v = v + acc_amt end
  elseif acc_tgt == 4 then
    local mid = math.ceil((n_sorted or 1) / 2)
    if pos_in_sorted == mid then v = v + acc_amt end
  end

  return clamp(math.floor(v), 1, 127)
end

--[[--------------------------------------------------------------------------
step_seconds(div_fraction, bpm, swing_mode, swing_pct, step_index)

Returns seconds to wait for a single step in an arpeggio sequence.

- div_fraction : e.g., 1/8 for eighth notes, 1/16 for sixteenth notes
- bpm          : clock.get_tempo()
- swing_mode   : 1=grid, 2=swing
- swing_pct    : 0..75 (%)
- step_index   : 1-based step counter for alternating swing
---------------------------------------------------------------------------]]
function Timing.step_seconds(div_fraction, bpm, swing_mode, swing_pct, step_index)
  local div = div_fraction or (1/8)
  local tempo = bpm or 120
  local base = (60 / tempo) * div

  if swing_mode == 2 and (swing_pct or 0) > 0 then
    local pct = (swing_pct or 0) / 100
    if ((step_index or 1) % 2) == 1 then
      return base * (1 + pct)
    else
      return base * (1 - pct)
    end
  end
  return base
end

return Timing
