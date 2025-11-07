-- lib/patternlib.lua
-- Pattern library for CHORDER ARP (split-out module)
-- Exports:
--   PatternLib.GENRES   -> { "Basics", "Pop", ... }
--   PatternLib.NAMES_FOR(genre) -> { pattern name strings }
--   PatternLib.GET(genre, idx)   -> { name=..., steps=[{d,oct,prob,r,rest}, ...], basics?, use_order?, legacy? }
--   PatternLib.RAND(genre)       -> returns a random pattern table for genre

local PatternLib = {}

-- ===== helpers =====
local function deg(d, oct, prob, r, rest)
  return { d=d, oct=oct or 0, prob=prob or 1.0, r=r or 1, rest=rest or false }
end

local function clone(tbl)
  local t = {}
  for k,v in pairs(tbl) do t[k] = v end
  return t
end

local function up1235(name)
  return { name=name, steps={deg(1),deg(2),deg(3),deg(5)} }
end

local function updown135(name)
  return { name=name, steps={deg(1),deg(3),deg(5),deg(3)} }
end

local function bounce15(name)
  return { name=name, steps={deg(1,0),deg(5,0),deg(1,1),deg(5,0)} }
end

local function walk1357(name)
  return { name=name, steps={deg(1),deg(3),deg(5),deg(7)} }
end

local function add_oct(template, shift)
  local t = clone(template)
  t.steps = {}
  for i,s in ipairs(template.steps) do
    t.steps[i] = { d=s.d, oct=(s.oct or 0)+(shift or 0), prob=s.prob, r=s.r, rest=s.rest }
  end
  t.name = template.name .. (shift>=0 and (" +"..shift.."O") or (" "..shift.."O"))
  return t
end

local function sprinkle_rests(template, every_n)
  local t = clone(template)
  t.steps = {}
  for i,s in ipairs(template.steps) do
    local rest = (every_n>0 and (i % every_n == 0)) or false
    t.steps[i] = { d=s.d, oct=s.oct, prob=s.prob, r=s.r, rest=rest }
  end
  t.name = template.name .. " · rests"
  return t
end

local function probabilistic(template, p)
  local t = clone(template)
  t.steps = {}
  for i,s in ipairs(template.steps) do
    t.steps[i] = { d=s.d, oct=s.oct, prob=p, r=s.r, rest=s.rest }
  end
  t.name = template.name .. string.format(" · p%.2f", p)
  return t
end

local function ratchet(template, r)
  local t = clone(template)
  t.steps = {}
  for i,s in ipairs(template.steps) do
    t.steps[i] = { d=s.d, oct=s.oct, prob=s.prob, r=r, rest=s.rest }
  end
  t.name = template.name .. (" · x"..r)
  return t
end

-- ===== seeds =====
local seeds = {
  Pop = {
    up1235("pop · up 1-2-3-5"),
    updown135("pop · 1-3-5-3"),
    bounce15("pop · bounce 1–5"),
    walk1357("pop · 1-3-5-7"),
    { name="pop · 1-6-4-5", steps={deg(1),deg(6),deg(4),deg(5)} },
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
      table.insert(lib[g], {
        name=(g.." · ladder 1→6 "..(o>=0 and "+"..o.."O" or o.."O")),
        steps={deg(1,o),deg(2,o),deg(3,o),deg(4,o),deg(5,o),deg(6,o),deg(5,o),deg(4,o)}
      })
    end
  end
  lib["Basics"] = basics_patterns()
  return lib
end

local LIB = expand()

local function genres_list()
  local t = {}
  for k,_ in pairs(LIB) do t[#t+1] = k end
  table.sort(t)
  return t
end

local function patterns_for(genre)
  local t = {}
  if LIB[genre] then for _,p in ipairs(LIB[genre]) do t[#t+1] = p.name end end
  return (#t>0) and t or {"(none)"}
end

local function get(genre, idx)
  local list = LIB[genre]
  if not list or #list==0 then return nil end
  idx = util.clamp(idx or 1, 1, #list)
  return list[idx]
end

local function random_for(genre)
  local list = LIB[genre]
  if not list or #list==0 then return nil end
  return list[math.random(1,#list)]
end

PatternLib.GENRES    = genres_list()
PatternLib.NAMES_FOR = patterns_for
PatternLib.GET       = get
PatternLib.RAND      = random_for

return PatternLib