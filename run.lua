local compile = require("compile")

local function print_usage_msg()
    print [[
usage: luajit run.lua <filename>

Options:
    -b      Dump bytecode informations.
]]
end

local filename

local args = {...}
local options = { bcprint = false }
local k = 1
while args[k] do
    if args[k] == "-b" then
        table.remove(args, k)
        options.bcprint = true
    else
        if string.sub(args[k], 1, 1) == "-" then
            print("Invalid option: ", args[k])
            print_usage_msg()
        end
        filename = args[k]
        k = k + 1
        if args[k] then print_usage_msg() end
    end
end

if not filename then print_usage_msg() end

-- Compute the bytecode string for the given filename.
local luacode = compile.file(filename, options)

if luacode then
    local fn = assert(loadstring(luacode))
    fn()
end
