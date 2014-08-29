local function foo(x)
    return x, x + 1, 2*x
end

local function prova(...)
	return ...
end

local function one(...)
	return (...)
end

local function single_value(x)
	return (foo(x))
end

local function alls(x)
	return foo(x)
end

print(foo(2))
print( (foo(2)) )

print(alls(foo(2)))
print(single_value(foo(2)))

print( one(foo(2)) )

print( prova(foo(2)) )
