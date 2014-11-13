local band, bor, lshift, bnot = bit.band, bit.bor, bit.lshift, bit.bnot
local strsub, strbyte, strchar, format = string.sub, string.byte, string.char, string.format

local BCDUMP = {
    HEAD1 = 0x1b,
    HEAD2 = 0x4c,
    HEAD3 = 0x4a,

    -- If you perform *any* kind of private modifications to the bytecode itself
    -- or to the dump format, you *must* set BCDUMP_VERSION to 0x80 or higher.
    VERSION = 1,

    -- Compatibility flags.
    F_BE    = 0x01,
    F_STRIP = 0x02,
    F_FFI   = 0x04,
}

BCDUMP.F_KNOWN = BCDUMP.F_FFI*2-1

local printer = {}

local function bytes_row(bytes, n)
    local t = {}
    local istart = (n - 1) * 8
    for i = istart + 1, istart + 8 do
        local b = bytes[i]
        if not b then break end
        t[#t+1] = format("%02x", b)
    end
    return #t, table.concat(t, " ")
end

local function text_fragment(text, n)
    local istart = (n - 1) * 46
    local s = strsub(text, istart + 1, istart + 46)
    return #s, s
end

function printer.write(self, ls, text)
    local n = 1
    local bcount, tlen = 0, 0
    repeat
        local alen, a = bytes_row(ls.bytes, n)
        local blen, b = text_fragment(text, n)
        print(format("%-24s| %s", a, b))
        bcount, tlen = bcount + alen, tlen + blen
        n = n + 1
    until bcount >= #ls.bytes and tlen >= #text
    ls.bytes = {}
end

local function byte(ls, p)
    p = p or ls.p
    local c = strsub(ls.data, p, p)
    -- print(">> BYTE", p, format("%02x", strbyte(c)))
    return strbyte(c)
end

local function bcread_need(ls, len)
	if ls.n < len then
        error("incomplete bytecode data")
    end
end

local function consume(ls, len)
    assert(ls.n >= len, "incomplete bytecode data")
    for p = ls.p, ls.p + len - 1 do
        ls.bytes[#ls.bytes + 1] = byte(ls, p)
    end
    ls.n = ls.n - len
end

local function bcread_dec(ls)
    assert(ls.n > 0, "incomplete bytecode data")
    local b = byte(ls)
    ls.bytes[#ls.bytes + 1] = b
	ls.n = ls.n - 1
    return b
end

local function bcread_byte(ls)
	local b = bcread_dec(ls)
	ls.p = ls.p + 1
	return b
end

local function bcread_uleb128(ls)
    local v = byte(ls)
    local p = ls.p + 1
    if v > 0x80 then
        local sh = 0
        v = band(v, 0x7f)
        repeat
            local b = byte(ls, p)
            v = bor(v, lshift(band(b, 0x7f), sh + 7))
            p, sh = p + 1, sh + 7
            bcread_dec(ls)
        until b < 0x80
    end
    bcread_dec(ls)
    ls.p = p
    return v
end

local function bcread_mem(ls, len)
    local s = strsub(ls.data, ls.p, ls.p + len - 1)
    consume(ls, len)
    ls.p = ls.p + len
    return s
end

local function flags_string(flags)
    local t = {}
    if band(flags, BCDUMP.F_FFI)   then t[#t+1] = "BCDUMP_F_FFI" end
    if band(flags, BCDUMP.F_STRIP) then t[#t+1] = "BCDUMP_F_STRIP" end
    return table.concat(t, "|")
end

local function bcread_header(ls)
    local flags
    -- bcread_want(ls, 3+5+5)
    if bcread_byte(ls) ~= BCDUMP.HEAD2 or bcread_byte(ls) ~= BCDUMP.HEAD3 or bcread_byte(ls) ~= BCDUMP.VERSION then
        return "invalid header"
    end
    printer:write(ls, "Header LuaJIT 2.0 BC")
    flags = bcread_uleb128(ls)
    printer:write(ls, format("Flags %s", flags_string(flags)))
    if band(flags, bnot(BCDUMP.F_KNOWN)) ~= 0 then
        return "unknown flags"
    end
    if band(flags, BCDUMP.F_STRIP) == 0 then
        local len = bcread_uleb128(ls)
        bcread_need(ls, len)
        local chunkname = bcread_mem(ls, len)
        printer:write(ls, format("Chunkname: %s", chunkname))
    end
end

local function bcread(s)
    local ls = {data = s, n = #s, p = 1, bytes = {}}
    local err
    if bcread_byte(ls) ~= BCDUMP.HEAD1 then
        return "invalid header beginning char"
    end
    return bcread_header(ls)
end

return { start = bcread }
