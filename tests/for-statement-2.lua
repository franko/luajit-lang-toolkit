local function foo(n)
	local sum = 0
	for i = n, n*n+n, n+1 do
		sum = sum + i*i
	end
	return sum
end

print(foo(10))
