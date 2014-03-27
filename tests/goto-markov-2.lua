-- Use a very weak pseudo-number generator just for testing purpose.
local function my_random(s)
    s.x = (16807 * s.x) % 2147483647
    return s.x % 50
end

local function foo(a)
    local s = { x = 13 }
    local ls = { }
    ::a::
        local x = 2*a + 1
        ls[#ls+1] = function() return x end
        if my_random(s) < 10 then goto a end
    ::b::
        ls[#ls+1] = function() return x end
        if my_random(s) < 20 then goto b end
    ::c::
        local y = x^2 + 1
        ls[#ls+1] = function() return y end
        if my_random(s) < 40 then goto a end
    return ls
end

local ls = foo(7)
for i = 1, #ls do
    print(ls[i]())
end
