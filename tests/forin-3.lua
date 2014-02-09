local function foo(ls)
	local sum = 0
	local fs = {}
	for i, v in ipairs(ls) do
		sum = sum + v*v
		fs[i] = function(x) return i + x end
	end
	return sum, fs
end

local sum, fs = foo {1, 3, 7, 12}
print(sum)
print(fs[1](7), fs[2](7), fs[3](7))
