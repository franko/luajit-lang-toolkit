local compile = require("lang.compile")
local bcsave = require("lang.bcsave")

local function usage()
  io.stderr:write[[
LuaJIT Language Toolkit usage: luajit [options]... [script [args]...].

Available options are:
  -b ...    Save or list bytecode.
]]
  os.exit(1)
end

local function check(success, result)
    if not success then
        io.stderr:write(result .. "\n")
        os.exit(1)
    else
        return result
    end
end

local args = {...}
local k = 1
while args[k] do
    local a = args[k]
    if type(a) == "string" and string.sub(a, 1, 2) == "-b" then
        local j = 1
        if #a > 2 then
            args[j] = "-" .. string.sub(a, 3)
            j = j + 1
        else
            table.remove(args, j)
        end
        bcsave.start(unpack(args))
        os.exit(0)
    else
        if string.sub(args[k], 1, 1) == "-" then
            print("Invalid option: ", args[k])
            print_usage_msg()
        end
        filename = args[k]
        k = k + 1
        if args[k] then usage() end
    end
end

if not filename then usage() end

-- Compute the bytecode string for the given filename.
local luacode = check(compile.file(filename))
local fn = assert(loadstring(luacode))
fn()
