local function foo(n)
	local sum = 0
	local fs = {}
	for i = 1, n do
		sum = sum + i*i
		fs[i] = function(x) return i + x end
	end
	return sum, fs
end

local s, fs = foo(10)
print(s)
print(fs[1](7), fs[2](7), fs[3](7))
