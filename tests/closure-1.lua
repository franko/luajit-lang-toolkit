local function foo(x, y)
    local a, b, c
    local function boo(z)
        a, b = z*z + 1, z - 1
    end
    boo(x * y - x + y)
    c = x + y
    return a * b * c
end

print(foo(3, 7))