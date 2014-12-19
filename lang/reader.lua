local strsub = string.sub

local function new_string_reader(src)
    local pos = 1
    local function reader()
        local chunk = strsub(src, pos, pos + 4096 - 32)
        pos = pos + #chunk
        return #chunk > 0 and chunk or nil
    end
    return reader
end

local function new_file_reader(filename)
    local f
    if filename then
        f = assert(io.open(filename, 'r'), "cannot open file " .. filename)
    else
        f = io.stdin
    end
    local function reader()
        return f:read(4096 - 32)
    end
    return reader
end

return { 
    string = new_string_reader,
    file = new_file_reader,
}
