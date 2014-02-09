local function foo(k)
	return k*k < 49
end

local sum = 0
local i = 1
while i < 10 and foo(i) do
    sum = sum + i*i
    i = i + 1
end
print(sum)
