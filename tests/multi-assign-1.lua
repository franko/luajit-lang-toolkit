local function foo(k)
    return k, k*k, 2*k + 1
end

local x = 3
local a, b, c = foo(x)
print(a, b, c)