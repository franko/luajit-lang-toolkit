local function boo(a, b)
	if b then return a else return a + 1 end
end

local function foo(x, y)
	local z = x + y
	return boo(z, x < y and x or y)
end

print(foo(3, 7))
