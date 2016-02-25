local function fact(n, accu)
	return n > 1 and fact(n-1, n * accu) or accu
end

print(fact(5, 1), fact(4, 1), fact(3, 1), fact(4, 5))
