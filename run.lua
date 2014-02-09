local compile = require("compile")
local filename = assert(..., "usage: luajit run.lua <filename>")

-- Compute the bytecode string for the given filename.
local luacode = compile.file(filename)

local fn = assert(loadstring(luacode))
fn()

