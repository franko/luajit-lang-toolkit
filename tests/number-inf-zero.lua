function foo(a)
    local x = 1/0
    local y = 0/1
    local w = a/0
    local z = 0/a
    print(x, y, w, z)
end

foo(7)

