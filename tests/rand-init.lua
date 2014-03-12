local function darray() return {} end
local iarray = darray

local rand, rand_init

if jit and jit.status and jit.status() then
  -- LJ2 has bit operations and zero-based arrays (internally).
  local bit = require("bit")
  local band, sar = bit.band, bit.arshift
  function rand_init(seed)
    local Rm, Rj, Ri = iarray(17), 16, 11
    for i=0,16 do Rm[i] = 0 end
    for i=16,0,-1 do
      seed = band(seed*9069, 0x7fffffff)
      Rm[i] = seed
    end
    function rand()
      local i = band(Ri+1, sar(Ri-16, 31))
      local j = band(Rj+1, sar(Rj-16, 31))
      Ri, Rj = i, j
      local k = band(Rm[i] - Rm[j], 0x7fffffff)
      Rm[j] = k
      return k * (1.0/2147483647.0)
    end
  end
else
  function rand_init(seed)
    local Rm, Rj = {}, 1
    for i=1,17 do Rm[i] = 0 end
    for i=17,1,-1 do
      seed = (seed*9069) % (2^31)
      Rm[i] = seed
    end
    function rand()
      local j, m = Rj, Rm
      local h = j - 5
      if h < 1 then h = h + 17 end
      local k = m[h] - m[j]
      if k < 0 then k = k + 2147483647 end
      m[j] = k
      if j < 17 then Rj = j + 1 else Rj = 1 end
      return k * (1.0/2147483647.0)
    end
  end
end

rand_init(15)
for k = 1, 10 do
  print(rand())
end
