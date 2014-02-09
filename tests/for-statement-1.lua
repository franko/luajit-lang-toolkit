local function foo(n)
	local sum = 0
	for i = 1, n do
		sum = sum + i*i
	end
	return sum
end

print(foo(10))
