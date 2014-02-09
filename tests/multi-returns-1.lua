local function foo(k)
	return k, k*k, 2*k+1
end

local function give(n)
	local k = 3*n + 2
	return foo(k)
end

print(give(5))
