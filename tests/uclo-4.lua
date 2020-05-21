local x = 1
local function f()
local y = 0
for i=1,100 do y=y+x end
    return y
end
print(f())
x = 2
print(f())
