local function fact(n, accu = 1)
	return n > 1 and fact(n-1, accu = n * accu) or accu
end

print(fact(5), fact(4), fact(3), fact(4, accu= 5))
