local function foo(a, b)
	local x
	if a < b  then x = 0 end
	if a > b  then x = 1 end
	if a <= b then x = 2 end
	if a >= b then x = 3 end
	if a == b then x = 4 end
	if a ~= b then x = 5 end
	if not (a < b) then x = 6 end
	return x
end

local x, y = 7, 3
print(foo(x, y), foo(y, x), foo(x, x))
