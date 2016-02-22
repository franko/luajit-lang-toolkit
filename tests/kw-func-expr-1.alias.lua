local f = function(x, mult, add)
	return x * mult + add
end

print(f(5, 3, 0), f(5, 2, 0), f(5, 3, 3), f(5, 7, 3))
