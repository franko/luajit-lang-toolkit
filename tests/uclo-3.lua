local z1, z2
local i = 1
repeat
    local j = i
    local function f() return j end
    if z1 then z2 = f else z1 = f end
    i = i + 1
until i > 10
print(z1())
print(z2())
