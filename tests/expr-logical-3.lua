local function foo(x, y)
	return (x < y and x*x or y*y) + 2*x*y + 7
end

print(foo(3, 7), foo(7, 3))
