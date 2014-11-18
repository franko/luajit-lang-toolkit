local bcread = require("lang.bcread")

local filename = ...
local f = io.open(filename, "rb")
local s = f:read("*a")
bcread.start(s)
f:close()
