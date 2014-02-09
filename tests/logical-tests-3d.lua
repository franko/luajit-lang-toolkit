local function foo(x, y)
	local c = (x < y)
	return c
end

local x, y = 3, 7
print(foo(x, y))
