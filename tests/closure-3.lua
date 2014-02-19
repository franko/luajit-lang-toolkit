local function foo(n)
    local f
    for k = n, n + 10 do
        if k % 7 == 0 then
            f = function(x) return k + x end
            break
        end
    end
    return f
end

local f = foo(1)
print(f(3))
