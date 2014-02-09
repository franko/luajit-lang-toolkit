local function init(x)
    return {name= 'foo', value = x*x}
end

local s = init(2.5)
local v = s.value
print(v)
