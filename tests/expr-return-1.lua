local function f1(x, y, z)
	return y
end

local function f2(x, y, z)
	return x, y, z
end

local function f3(x, y, z)
	return x + y + z
end

local u = 7
local function f4(x)
	return x + u
end

local function f5(x)
	return u
end

local a, b, c = 3, 5, 21
print(f1(a, b, c))
print(f2(a, b, c))
print(f3(a, b, c))
print(f4(a))
print(f5(a))
