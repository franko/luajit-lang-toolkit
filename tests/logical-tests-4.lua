local function foo(a, b)
	return not (a + b)
end

local x, y = 7, 3
print(foo(x, y), foo(y, x))
