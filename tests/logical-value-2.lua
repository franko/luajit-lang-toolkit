local function foo(x, y)
    if x < y and x == y then
        return x
    else
        return y
    end
end

local function boo(x, y)
    if x < y or x == y then
        return x
    else
        return y
    end
end

local x, y = 3, 7
print(foo(x, y), foo(y, x))
print(boo(x, y), boo(y, x))
