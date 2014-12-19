local function foo(x)
    return x, 2*x, x^2-1
end

local function boo(x)
    local arg = { foo(x) }
    for k = 1, #arg do
        print(arg[k])
    end
end

boo(7)

