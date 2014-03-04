local function foo1(x, y)
    return x < y and x + y
end

local function foo1bis(x, y, somef)
    return x < y and somef(x, y)
end

local function foo2(x, y)
    return x < y and x == y
end

local function foo3(x, y)
    return x < y or x - y
end

local function foo4(x, y)
    return x < y or x == y
end

local function diff(a, b) return a - b end

local x, y = 3, 7
print(foo1(x, y), foo1(y, x))
print(foo1bis(x, y, diff), foo1bis(y, x, diff))
print(foo2(x, y), foo2(y, x))
print(foo3(x, y), foo3(y, x))
print(foo4(x, y), foo4(y, x))