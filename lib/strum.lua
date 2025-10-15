-- lib/strum.lua
-- Strum pattern generator for CHORDER
-- Returns orderings for a given count and strum type.
-- API: Strum.make(count, stype, state) -> order_tbl, updated_state

local M = {}

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

local function edge_kiss(n)
  return outside_in(n)
end

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

-- Dispatcher
-- stype mapping:
-- 1 up, 2 down, 3 up/down, 4 down/up, 5 random, 6 center-out, 7 outside-in,
-- 8 bass-bounce, 9 treble-bounce, 10 random no-repeat first, 11 random stable ends,
-- 12 edge-kiss, 13 ping-pair, 14 arp chunk 2–3, 15 guitar rake, 16 harp gliss split,
-- 17 arp chunk 2–3 ↓, 18 guitar rake ↓, 19 harp gliss split (interleaved)
function M.make(count, stype, state)
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
  elseif stype == 10 then
    local tries = 0
    repeat shuffle_inplace(order); tries = tries + 1 until (order[1] ~= state.last_first) or tries > 8
    state.last_first = order[1]; state.last_last  = order[#order]; return order, state
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
    state.last_first = order[1]; state.last_last  = order[#order]; return order, state
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

return M
