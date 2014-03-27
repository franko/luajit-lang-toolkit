-- Use a very weak pseudo-number generator just for testing purpose.
local function my_random(s)
    s.x = (16807 * s.x) % 2147483647
    return s.x % 50
end

local function markov()
    local s = { x = 13 }
    ::a::
        print('A')
        if my_random(s) < 10 then goto c end
    ::b::
        print('B')
        if my_random(s) < 20 then goto d end
    ::c::
        print('C')
        if my_random(s) < 5  then goto a end
    ::d::
        print('D')
        if my_random(s) < 48  then goto b else return end
end

markov()
