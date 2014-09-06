local function foo(a, b, c, d)
    a = a or 1
    b = b + 2*d + c
    c = c + 1
    return a + b*c + 2*d
end

local sum = 0
for k = 1, 400 do
    sum = sum + foo(k, -2, 3, k + 1)
end
print(sum)

