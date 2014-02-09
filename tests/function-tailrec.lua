local function fact(n, accu)
	if n <= 1 then
		return accu
	else
		return fact(n - 1, n * accu)
	end
end

print(fact(5, 1))
