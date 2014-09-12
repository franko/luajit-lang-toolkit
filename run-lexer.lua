local lex_setup = require("lexer")
local reader = require("reader")
local filename = assert(..., "usage: luajit run-lexer.lua <filename>")

local ls = lex_setup(reader.file(filename), filename)

repeat
    ls:next()
    if ls.tokenval then
        print(ls.token, ls.tokenval)
    else
        print(ls.token)
    end
until ls.token == "TK_eof"
