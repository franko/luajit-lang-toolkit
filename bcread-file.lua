local bcread = require("lang.bcread")

local filename = ...
local f = io.open(filename, "r")
local s = f:read("*a")
bcread.start(s)
f:close()
