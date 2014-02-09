local function squares_iter(ls, i)
	if i + 1 <= #ls then
		local v = ls[i + 1]
		return i + 1, v, v*v
	end
end

local function squares(ls)
	return squares_iter, ls, 0
end

local function foo(ls)
	local s, ssq = 0, 0
	for i, v, vsq in squares(ls) do
		s = s + v
		ssq = ssq + vsq
	end
	return s, ssq
end

print(foo {3, 7, -2, 5})
