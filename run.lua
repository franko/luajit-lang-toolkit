local function usage()
  io.stderr:write[[
LuaJIT Language Toolkit usage: luajit [options]... [script [args]...].

Available options are:
  -b ...    Save or list bytecode.
  -c ...    Generate Lua code and run.
            If followed by the "v" option the generated Lua code
            will be printed.
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
local opt = {}
local k = 1
while args[k] do
    local a = args[k]
    if string.sub(args[k], 1, 1) == "-" then
        if string.sub(a, 2, 2) == "b" then
            local j = 1
            if #a > 2 then
                args[j] = "-" .. string.sub(a, 3)
                j = j + 1
            else
                table.remove(args, j)
            end
            require("lang.bcsave").start(unpack(args))
            os.exit(0)
        elseif string.sub(a, 2, 2) == "c" then
            opt.code = true
            local copt = string.sub(a, 3, 3)
            if copt == "v" then
                opt.debug = true
            elseif copt ~= "" then
                print("Invalid Lua code option: ", copt)
                usage()
            end
        elseif string.sub(a, 2, 2) == "v" then
            opt.debug = true
        else
            print("Invalid option: ", args[k])
            usage()
        end
    else
        filename = args[k]
    end
    k = k + 1
end

if not filename then usage() end

local compile = require("lang.compile")

-- Compute the bytecode string for the given filename.
local luacode = check(compile.file(filename, opt))
if opt.debug then
    print(luacode)
    print('\n\nOutput:')
end
local fn = assert(loadstring(luacode))
fn()

