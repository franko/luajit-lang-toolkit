local format = string.format

local function printf(fmt, ...)
    print(format(fmt, ...))
end

local name = "Ciccio"
printf("Ciao %s, come stai ?", name)
local a, b = 3.25, 225
printf("Some numbers, float: %g, integer: %i", a, b)

