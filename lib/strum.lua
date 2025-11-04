-- lib/strum.lua
-- Flat strum type list (no families), compatible with chorder.lua
-- Exposes:
--   Strum.TYPES                 -> ordered names
--   Strum.index_by_name(name)   -> index
--   Strum.make(n, idxOrName, state) -> orderTbl, state

local Strum = {}

-- ===== utilities =====
local function clamp(v, lo, hi) if v < lo then return lo elseif v > hi then return hi else return v end end
local function reverse_inplace(t) local i, j = 1, #t; while i < j do t[i], t[j] = t[j], t[i]; i = i + 1; j = j - 1 end end
local function shuffle_inplace(t) for i = #t, 2, -1 do local j = math.random(i); t[i], t[j] = t[j], t[i] end end
local function range_up(n) local t = {}; for i=1,n do t[i]=i end; return t end
local function range_down(n) local t = {}; local k=1; for i=n,1,-1 do t[k]=i; k=k+1 end; return t end

-- ===== base patterns (keep original behavior) =====
local function seq_up(n) return range_up(n) end
local function seq_down(n) return range_down(n) end

local function seq_up_down(n)
  if n <= 1 then return {1} end
  local t = {}; for i=1,n do t[#t+1] = i end
  for i=n-1,2,-1 do t[#t+1] = i end
  return t
end

local function seq_down_up(n)
  if n <= 1 then return {1} end
  local t = {}; for i=n,1,-1 do t[#t+1] = i end
  for i=2,n-1 do t[#t+1] = i end
  return t
end

local function seq_random(n) local t = range_up(n); shuffle_inplace(t); return t end

local function seq_center_out(n)
  if n == 1 then return {1} end
  local c = math.floor((n+1)/2)
  local out, L, R = {c}, c-1, c+1
  while (#out < n) do
    if R <= n then out[#out+1] = R; R = R + 1 end
    if #out >= n then break end
    if L >= 1 then out[#out+1] = L; L = L - 1 end
  end
  return out
end

local function seq_outside_in(n)
  if n <= 2 then return range_up(n) end
  local out, L, R = {}, 1, n
  while L <= R do
    out[#out+1] = L
    if L < R then out[#out+1] = R end
    L = L + 1; R = R - 1
  end
  return out
end

local function seq_bass_bounce(n)
  if n <= 1 then return {1} end
  local t = {1}
  for i=2,n do t[#t+1] = i end
  t[#t+1] = 1
  return t
end

local function seq_treble_bounce(n)
  if n <= 1 then return {1} end
  local t = {n}
  for i=n-1,1,-1 do t[#t+1] = i end
  t[#t+1] = n
  return t
end

local function seq_random_no_repeat_first(n)
  if n <= 1 then return {1} end
  local rest = {}
  for i=2,n do rest[#rest+1]=i end
  shuffle_inplace(rest)
  local t = {1}
  for _,i in ipairs(rest) do t[#t+1]=i end
  return t
end

local function seq_random_stable_ends(n)
  if n <= 2 then return range_up(n) end
  local mid = {}
  for i=2,n-1 do mid[#mid+1] = i end
  shuffle_inplace(mid)
  local t = {1}
  for _,i in ipairs(mid) do t[#t+1] = i end
  t[#t+1] = n
  return t
end

local function seq_edge_kiss(n)
  if n <= 2 then return range_up(n) end
  local t = {}
  local L, R = 1, n
  while L <= R do
    t[#t+1] = L
    if L < R then t[#t+1] = R end
    L = L + 1; R = R - 1
  end
  return t
end

local function seq_ping_pair(n)
  if n <= 1 then return {1} end
  local t, i = {}, 1
  while i < n do
    t[#t+1] = i
    t[#t+1] = i+1
    i = i + 2
  end
  if n % 2 == 1 then t[#t+1] = n end
  return t
end

local function seq_arp_chunk_23(n)
  if n <= 1 then return {1} end
  local t, i = {}, 1
  while i <= n do
    t[#t+1] = i
    if i+1 <= n then t[#t+1] = i+1 end
    if i+2 <= n then t[#t+1] = i+2 end
    i = i + 2
  end
  return t
end

local function seq_guitar_rake(n) return range_up(n) end
local function seq_harp_gliss_split(n)
  local t, half = {}, math.floor(n/2)
  for i=1,half do t[#t+1]=i end
  for i=half+1,n do t[#t+1]=i end
  return t
end
local function seq_arp_chunk_23_down(n) local t = seq_arp_chunk_23(n); reverse_inplace(t); return t end
local function seq_guitar_rake_down(n) local t = seq_guitar_rake(n); reverse_inplace(t); return t end

local function seq_harp_gliss_split_interleaved(n)
  if n <= 2 then return range_up(n) end
  local half = math.floor(n/2)
  local t, i, j = {}, 1, half+1
  while i <= half or j <= n do
    if i <= half then t[#t+1] = i; i = i + 1 end
    if j <= n   then t[#t+1] = j; j = j + 1 end
  end
  return t
end

-- ===== Additional patterns 20..29 =====
local function seq_bass_random(n)
  if n <= 1 then return {1} end
  local rest = {}
  for i=2,n do rest[#rest+1] = i end
  shuffle_inplace(rest)
  table.insert(rest, 1, 1)
  return rest
end

local function seq_top_random(n)
  if n <= 1 then return {1} end
  local rest = {}
  for i=1,n-1 do rest[#rest+1] = i end
  shuffle_inplace(rest)
  table.insert(rest, 1, n)
  return rest
end

local function seq_outer_random_mid(n)
  if n == 1 then return {1} end
  if n == 2 then return {1,2} end
  local mid = {}
  for i=2,n-1 do mid[#mid+1] = i end
  shuffle_inplace(mid)
  local out = {1, n}
  for _,i in ipairs(mid) do out[#out+1] = i end
  return out
end

local function seq_weave_lo_hi(n)
  local out, i, j = {}, 1, n
  while i <= j do
    out[#out+1] = i
    if i < j then out[#out+1] = j end
    i = i + 1; j = j - 1
  end
  return out
end

local function seq_weave_hi_lo(n)
  local out, i, j = {}, 1, n
  while i <= j do
    out[#out+1] = j
    if i < j then out[#out+1] = i end
    i = i + 1; j = j - 1
  end
  return out
end

local function seq_inside_out_alt(n)
  local c = math.floor((n+1)/2)
  local L, R = c-1, c+1
  local out, take_right = {c}, true
  while #out < n do
    if take_right and R <= n then out[#out+1] = R; R = R + 1
    elseif (not take_right) and L >= 1 then out[#out+1] = L; L = L - 1
    elseif R <= n then out[#out+1] = R; R = R + 1
    elseif L >= 1 then out[#out+1] = L; L = L - 1 end
    take_right = not take_right
  end
  return out
end

local function seq_inside_out_rand(n)
  local c = math.floor((n+1)/2)
  local L, R = c-1, c+1
  local out = {c}
  while #out < n do
    if L < 1 then out[#out+1] = R; R = R + 1
    elseif R > n then out[#out+1] = L; L = L - 1
    else
      if math.random() < 0.5 then out[#out+1] = L; L = L - 1
      else out[#out+1] = R; R = R + 1 end
    end
  end
  return out
end

local function seq_odds_then_evens_up(n)
  local out = {}
  for i=1,n,2 do out[#out+1] = i end
  for i=2,n,2 do out[#out+1] = i end
  return out
end

local function seq_evens_then_odds_up(n)
  local out = {}
  for i=2,n,2 do out[#out+1] = i end
  for i=1,n,2 do out[#out+1] = i end
  return out
end

local function seq_lowhalf_up_highhalf_down(n)
  local mid = math.floor(n/2)
  local out = {}
  for i=1, mid do out[#out+1] = i end
  for i=n, mid+1, -1 do out[#out+1] = i end
  return out
end

-- ===== Public type list (flat, ordered) =====
Strum.TYPES = {
  -- Classic
  "up", "down", "up/down", "down/up", "random",
  -- Contour
  "center-out", "outside-in", "bass-bounce", "treble-bounce",
  -- Variants / extras
  "random (no repeat first)",         -- 10
  "random (stable ends)",             -- 11
  "edge-kiss",                        -- 12
  "ping-pair",                        -- 13
  "arp-chunk 2-3",                    -- 14
  "guitar-rake",                      -- 15
  "harp-gliss split",                 -- 16
  "arp-chunk 2-3 (down)",             -- 17
  "guitar-rake (down)",               -- 18
  "harp-gliss interleaved",           -- 19
  "bass-random",                      -- 20
  "top-random",                       -- 21
  "outer random → mid",               -- 22
  "weave lo→hi",                      -- 23
  "weave hi→lo",                      -- 24
  "inside-out (alt)",                 -- 25
  "inside-out (random)",              -- 26
  "odds then evens",                  -- 27
  "evens then odds",                  -- 28
  "low-half up / high-half down",     -- 29
}

-- Map names -> indices (case-insensitive)
local NAME_TO_INDEX = (function()
  local m = {}
  for i, name in ipairs(Strum.TYPES) do m[string.lower(name)] = i end
  return m
end)()

-- For chorder.lua compatibility
function Strum.index_by_name(name)
  if type(name) ~= "string" then return 1 end
  return NAME_TO_INDEX[string.lower(name)] or 1
end

-- Factory: accepts number OR name
function Strum.make(n, idx_or_name, state)
  n = clamp(n or 0, 0, 64)
  if n <= 0 then return {}, (state or { alt_flip=false, last_first=nil, last_last=nil }) end
  state = state or { alt_flip=false, last_first=nil, last_last=nil }

  local stype = idx_or_name
  if type(idx_or_name) == "string" then
    stype = Strum.index_by_name(idx_or_name)
  elseif type(idx_or_name) ~= "number" then
    stype = 1
  end
  stype = clamp(math.floor(stype), 1, #Strum.TYPES)

  if     stype == 1  then return seq_up(n), state
  elseif stype == 2  then return seq_down(n), state
  elseif stype == 3  then return seq_up_down(n), state
  elseif stype == 4  then return seq_down_up(n), state
  elseif stype == 5  then return seq_random(n), state
  elseif stype == 6  then return seq_center_out(n), state
  elseif stype == 7  then return seq_outside_in(n), state
  elseif stype == 8  then return seq_bass_bounce(n), state
  elseif stype == 9  then return seq_treble_bounce(n), state
  elseif stype == 10 then return seq_random_no_repeat_first(n), state
  elseif stype == 11 then return seq_random_stable_ends(n), state
  elseif stype == 12 then return seq_edge_kiss(n), state
  elseif stype == 13 then return seq_ping_pair(n), state
  elseif stype == 14 then return seq_arp_chunk_23(n), state
  elseif stype == 15 then return seq_guitar_rake(n), state
  elseif stype == 16 then return seq_harp_gliss_split(n), state
  elseif stype == 17 then return seq_arp_chunk_23_down(n), state
  elseif stype == 18 then return seq_guitar_rake_down(n), state
  elseif stype == 19 then return seq_harp_gliss_split_interleaved(n), state
  elseif stype == 20 then return seq_bass_random(n), state
  elseif stype == 21 then return seq_top_random(n), state
  elseif stype == 22 then return seq_outer_random_mid(n), state
  elseif stype == 23 then return seq_weave_lo_hi(n), state
  elseif stype == 24 then return seq_weave_hi_lo(n), state
  elseif stype == 25 then return seq_inside_out_alt(n), state
  elseif stype == 26 then return seq_inside_out_rand(n), state
  elseif stype == 27 then return seq_odds_then_evens_up(n), state
  elseif stype == 28 then return seq_evens_then_odds_up(n), state
  elseif stype == 29 then return seq_lowhalf_up_highhalf_down(n), state
  end
  return seq_up(n), state
end

return Strum
