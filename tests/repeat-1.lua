local function foo(n)
	local i, sum = 1, 0
	repeat
		sum = sum + i*i
		i = i + 1
	until i > 10
	return sum
end

print(foo(10))
