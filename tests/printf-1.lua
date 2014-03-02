
local s1 = "Hello"
local s2 = "Ciccio"

local format = string.format

local some_table = { pi = 3.14 }

local function printf(...)
  io.write(format(...))
end

printf("I say: %s %s!\n", s1, s2)
