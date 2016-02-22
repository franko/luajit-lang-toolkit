local f = function(x, mult = 3, add = 0)
	return x * mult + add
end

print(f(5), f(5, mult = 2), f(5,add=3), f(5, mult=7, add=3))
