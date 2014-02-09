local function foo(x, y)
	if x < y then
		return x + y
	elseif x + y < x - y then
		return x - y
	elseif x * y < x / y then
		return x * y
	else
		return x / y
	end
end

print(foo(3, 4), foo(4, 3))
