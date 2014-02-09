local lex_setup = require('lexer')
local parse = require('parser')
local ast = require('lua-ast').New()
local generator = require('generator')

local strsub = string.sub

local function new_string_reader()
    local src, pos
    local function init(src_in)
        src, pos = src_in, 1
    end
    local function reader()
        local chunk = strsub(src, pos, pos + 4096 - 32)
        pos = pos + #chunk
        return #chunk > 0 and chunk or nil
    end
    return init, reader
end

local function new_file_reader()
    local f
    local function init(filename)
        f = assert(io.open(filename, 'r'), "cannot open file " .. filename)
    end
    local function reader()
        return f:read(4096 - 32)
    end
    return init, reader
end

local string_reader_init, string_reader = new_string_reader()
local file_reader_init, file_reader = new_file_reader()

local function compile(reader, filename)
    local ls = lex_setup(reader, filename)
    local tree = parse(ast, ls)
    local luacode = generator(tree, filename)

    -- dump the bytecode
    local jbc = require("jit.bc")
    local fn = assert(loadstring(luacode))
    jbc.dump(fn, nil, true)

    return luacode
end

local function lang_loadstring(src, filename)
    string_reader_init(src)
    return compile(string_reader, filename)
end

local function lang_loadfile(filename)
    file_reader_init(filename)
    return compile(file_reader, filename)
end

-- Used for the "use" statement to query the symbols defined in a given
-- module.
function ast.probe(name)
    local defs = { }
    local mod = _G[name]
    for k, v in pairs(mod) do
        defs[k] = true
    end
    return defs
end

return { string = lang_loadstring, file = lang_loadfile }

