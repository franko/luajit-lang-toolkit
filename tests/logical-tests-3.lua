local function foo(x, y)
	local a = (x < y) and (x*x < y*y)
	return a
end

print(foo(3, 7), foo(7, 3))
