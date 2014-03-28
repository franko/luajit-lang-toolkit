local RANDOM_SEED = 101009 -- Must be odd.

------------------------------------------------------------------------------
-- This is a Lagged Fibonacci Pseudo-random Number Generator with
-- j, k, M = 5, 17, 31. Pretty weak, but same as C/Java SciMark.
------------------------------------------------------------------------------

local rand

local function rand_init(seed)
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

local function random_vector(n)
  local v = {}
  for x=1,n do v[x] = rand() end
  return v
end

local function quasi_diag_randmat(n, alpha)
  local a = {}
  for y = 1, n do
    local v = {}
    a[y] = v
    for x = 1, n do
      local z = rand()
      v[x] = x == y and z or alpha * z / n
    end
  end
  return a
end

local function sor_iter(A, n, b, x, om)
  for i = 1, n do
    local sig = 0
    local Ai = A[i]
    for j = 1, n do
      if i ~= j then
        sig = sig + Ai[j] * x[j]
      end
    end
    x[i] = x[i] + om * ((b[i] - sig) / Ai[i] - x[i])
  end
end

local function print_vector(v, n)
  for i = 1, n do
    io.write(string.format("%12g ", v[i]))
  end
  io.write("\n")
end

rand_init(RANDOM_SEED)

local N = 100
local A = quasi_diag_randmat(N, 0.1)
local b = random_vector(N)
local x = {}
for i = 1, N do x[i] = b[i] / A[i][i] end

local xp = {}
for i = 1, N do xp[i] = 0 end

for cycle = 1, 100 do
  sor_iter(A, N, b, x, 1.25)

  local diff = 0
  for i = 1, N do
    local d = (x[i] - xp[i])^2
    xp[i] = x[i]
    diff = diff + d
  end

  diff = math.sqrt(diff)

  print(string.format("Iteration: %4i, Residual: %14g", cycle, diff))

  if diff < 1.0e-6 then break end
end

print("Solution:")
print_vector(x, N)

local res = 0
for i = 1, N do
  local t = 0
  local Ai = A[i]
  for j = 1, N do t = t + Ai[j] * x[j] end
  res = res + (t - b[i])^2
end

print(string.format("Exactness residual: %g", res))
