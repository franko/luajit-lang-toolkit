local function foo(x, y)
    return x < y and x*x or y*y
end

local x, y = 3, 7
print(foo(x, y))
