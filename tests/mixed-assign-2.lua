local function foo(k)
    return k*k, 2*k + 1
end

local x = 3
local a, b, c
a, b, c = 13, foo(x)
print(a, b, c)
