local function multi(...)
	return ...
end

local function onearg1(x, ...)
	print("onearg1", x)
	return x, ...
end

local function onearg2(x, ...)
	print("onearg2", x)
	return ...
end

local a, b, c = 3, 7, 21
print(multi(a, b, c))
print(onearg1(a, b, c))
print(onearg2(a, b, c))
