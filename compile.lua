local lex_setup = require('lexer')
local parse = require('parser')
local ast = require('lua-ast').New()
local generator = require('generator')
local reader = require('reader')

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
    reader.string_init(src)
    return compile(reader.string, filename)
end

local function lang_loadfile(filename)
    reader.file_init(filename)
    return compile(reader.file, filename)
end

return { string = lang_loadstring, file = lang_loadfile }
