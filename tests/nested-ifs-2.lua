local function foo(x, y)
	if x < y then
		if x*x < y*y then
			return x*x
		else
			return y*y
		end
	else
		if x + y < x - y then
			return x
		else
			return y
		end
	end
end

print(foo(3, 4), foo(4, 3))
