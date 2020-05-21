local z1, z2
for i=1,10 do
    local function f() return i end
    if z1 then z2 = f else z1 = f end
end
print(z1())
print(z2())
