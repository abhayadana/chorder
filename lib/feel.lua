-- lib/feel.lua
-- Timing, humanize, swing, and velocity-shaping helpers
-- Safe to require from any Norns script (no side effects).
-- Exposes:
--   step_seconds(div, bpm, swing_mode, swing_pct, step_index)
--   step_offsets_zero_index(n, strum_steps, shape, amt, skip_steps)
--   step_offsets_one_index(n, strum_steps, shape, amt, skip_steps)
--   apply_velocity(k, total, base_vel, pos_in_sorted, n_sorted, ramp_per_step, accent_target, accent_amt)

local Feel = {}

-- ===== internal helpers =====
local function clamp01(x) return math.min(1, math.max(0, x or 0)) end
local function iround(x) return math.floor((x or 0) + 0.5) end

-- normalized position helpers (0..1)
local function t_pos(i, total_gaps)
  if total_gaps <= 1 then return 0 end
  return (i - 1) / (total_gaps - 1)
end

-- weights for gap i (between event i and i+1)
local function gap_weight(i, gaps, shape, amt, skip_steps)
  local a = clamp01((amt or 0) / 100)
  local t = t_pos(i, gaps)

  -- base weight is 1.0 (straight)
  local w = 1.0

  if shape == 2 then
    -- Serpentine: longer at ends, shorter in middle
    -- | ends=1+a | center=1
    w = 1.0 + a * math.abs(2 * t - 1)

  elseif shape == 3 then
    -- Accelerando: progressively smaller gaps
    -- larger at start -> smaller at end
    w = 1.0 - a * t

  elseif shape == 4 then
    -- Ritardando: progressively larger gaps
    -- smaller at start -> larger at end
    w = 1.0 + a * t

  elseif shape == 5 then
    -- Rake ease-in: start slow (bigger gaps), end fast (small gaps)
    -- quadratic in t (0 -> 1)
    w = 1.0 - a * (t * t)

  elseif shape == 6 then
    -- Rake ease-out: start fast, end slow
    -- inverse quadratic
    w = 1.0 + a * (t * t)

  elseif shape == 7 then
    -- Skip alt gaps: alternate extra skip added (handled below)
    w = 1.0
  end

  -- Skip alternate gaps by adding discrete "skip" steps to every other gap.
  -- This is applied in the caller by adding integer steps; here we express it
  -- as a proportional increase so that weighting still interacts sensibly.
  if shape == 7 and skip_steps and skip_steps > 0 then
    if (i % 2) == 0 then
      -- bump even gaps
      -- Convert discrete step skip into relative scale against a typical base step (assume 1).
      -- The caller will later add the real integer skip on top of rounded gaps.
      w = w + a
    end
  end

  return math.max(0, w)
end

-- Build cumulative integer step offsets with weights per gap.
-- Returns a table of length n (events) with offsets in *integer steps*.
local function build_offsets(n, base_steps, shape, amt, skip_steps, zero_index)
  local offsets = {}
  if n <= 0 then return offsets end
  local gaps = math.max(0, n - 1)

  -- First offset is zero
  if zero_index then
    -- offsets[1] corresponds to event index 0 in the caller's mind
    offsets[1] = 0
  else
    -- one-indexed sequence also starts at zero for first note
    offsets[1] = 0
  end

  if (not base_steps) or base_steps <= 0 or gaps == 0 then
    -- Everything stacked, but still zero-start
    for i = 2, n do offsets[i] = 0 end
    return offsets
  end

  for i = 1, gaps do
    local w = gap_weight(i, gaps, shape or 1, amt or 0, skip_steps or 0)
    local gap_steps = iround(base_steps * w)

    -- Apply discrete skipping for shape 7 explicitly in steps
    if (shape == 7) and (skip_steps or 0) > 0 then
      if (i % 2) == 0 then
        gap_steps = gap_steps + skip_steps
      end
    end

    local prev = offsets[i] or 0
    offsets[i + 1] = prev + math.max(0, gap_steps)
  end

  return offsets
end

-- ===== public API =====

-- Return seconds for a single step given div, bpm, and swing settings.
-- swing_mode: 1 = "grid" (no swing), 2 = "swing %"
-- swing_pct:  0..75 (%)
-- step_index: 1-based index of the current step to alternate swing phase.
function Feel.step_seconds(div, bpm, swing_mode, swing_pct, step_index)
  local tempo = bpm or 120
  local division = div or 1/4
  local base = (60 / tempo) * division

  if (swing_mode or 1) ~= 2 then
    return base
  end

  local s = clamp01((swing_pct or 0) / 100)
  -- Alternate long/short by step parity
  local odd = ((step_index or 1) % 2) == 1
  if odd then
    return base * (1 + s)
  else
    return base * (1 - s)
  end
end

-- Offsets in integer *steps*, zero-index expectation for callers who do e[k-1].
-- (First event offset=0, second event offset >= 0, etc.)
function Feel.step_offsets_zero_index(n, strum_steps, shape, amt, skip_steps)
  return build_offsets(n or 0, strum_steps or 0, shape or 1, amt or 0, skip_steps or 0, true)
end

-- Offsets in integer *steps*, one-index expectation (callers use offs[k]).
-- (First event offset=0, second event offset >= 0, etc.)
function Feel.step_offsets_one_index(n, strum_steps, shape, amt, skip_steps)
  return build_offsets(n or 0, strum_steps or 0, shape or 1, amt or 0, skip_steps or 0, false)
end

-- Velocity shaping with per-step ramp and target accent.
-- accent_target: 1=none, 2=bass, 3=top, 4=middle
function Feel.apply_velocity(k, total, base_vel, pos_in_sorted, n_sorted, ramp_per_step, accent_target, accent_amt)
  local v = (base_vel or 100) + ((k - 1) * (ramp_per_step or 0))

  local tgt = accent_target or 1
  local acc = accent_amt or 0

  if tgt ~= 1 and (n_sorted or 0) > 0 then
    if tgt == 2 then
      -- bass (lowest)
      if (pos_in_sorted or 1) == 1 then v = v + acc end
    elseif tgt == 3 then
      -- top (highest)
      if (pos_in_sorted or 1) == n_sorted then v = v + acc end
    elseif tgt == 4 then
      -- middle (closest to center)
      local mid = math.floor((n_sorted + 1) / 2)
      if (pos_in_sorted or 1) == mid then v = v + acc end
    end
  end

  -- Clamp to MIDI range
  if util and util.clamp then
    v = util.clamp(v, 1, 127)
  else
    v = math.max(1, math.min(127, v))
  end
  return v
end

return Feel
