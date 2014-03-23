-- Use a very weak pseudo-number generator just for testing purpose.
local function my_random(s)
    s.x = (16807 * s.x) % 2147483647
    return s.x
end

local function my_iter(s, i)
	if i < 20 then return i + 1, my_random(s) end
end

local function foo()
	local s = { x = 13 }
	for i, value in my_iter, s, 0 do
		print(i, value)
	end
end

foo()
