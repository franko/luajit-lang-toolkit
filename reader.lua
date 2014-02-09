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

return { 
    string_init = string_reader_init,
    string = string_reader,
    file_init = file_reader_init,
    file = file_reader,
}
