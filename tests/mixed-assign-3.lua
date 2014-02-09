local function foo(k)
    return k*k, 2*k + 1
end

local a, b
a, b = 13, foo(3)
print(a, b)
