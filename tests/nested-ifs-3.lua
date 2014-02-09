local function foo(x, y)
	if x < y then
		if x*x < y*y then
			if x + y < x - y then
				return x
			else
				return y
			end
		else
			return y*y
		end
	else
		return x + y
	end
end

print(foo(3, 4), foo(4, 3))
