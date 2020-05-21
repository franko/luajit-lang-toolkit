local z1
local i = 1
while i <= 2 do
  local j = i
  z1 = z1 or function() return j end
  i = i + 1
end
print(z1())
