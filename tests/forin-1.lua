local function foo(ls)
	local sum = 0
	for i, v in ipairs(ls) do
		sum = sum + v*v
	end
	return sum
end

print(foo {1, 3, 7, 12})
