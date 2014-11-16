local ffi = require("ffi")

local band, bor, lshift, rshift, bnot = bit.band, bit.bor, bit.lshift, bit.rshift, bit.bnot
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

local BCDUMP_KGC_CHILD, BCDUMP_KGC_TAB, BCDUMP_KGC_I64, BCDUMP_KGC_U64, BCDUMP_KGC_COMPLEX, BCDUMP_KGC_STR = 0, 1, 2, 3, 4, 5

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

function printer.write(self, ls, fmt, ...)
    local n = 1
    local bcount, tlen = 0, 0
    local text = format(fmt, ...)
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
    return strbyte(c)
end

local function bcread_need(ls, len)
    if ls.n < len then
        error("incomplete bytecode data")
    end
end

local function bcread_consume(ls, len)
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
    if v >= 0x80 then
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

-- Read top 32 bits of 33 bit ULEB128 value from buffer.
local function bcread_uleb128_33(ls)
    local v = rshift(byte(ls), 1)
    local p = ls.p + 1
    if v >= 0x40 then
        local sh = -1
        v = band(v, 0x3f)
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
    bcread_consume(ls, len)
    ls.p = ls.p + len
    return s
end

local bcread_block = bcread_mem

local function flags_string(flags)
    local t = {}
    if band(flags, BCDUMP.F_FFI) ~= 0 then t[#t+1] = "BCDUMP_F_FFI" end
    if band(flags, BCDUMP.F_STRIP) ~= 0 then t[#t+1] = "BCDUMP_F_STRIP" end
    return #t > 0 and table.concat(t, "|") or "None"
end

local function bcread_header(ls)
    if bcread_byte(ls) ~= BCDUMP.HEAD2 or bcread_byte(ls) ~= BCDUMP.HEAD3 or bcread_byte(ls) ~= BCDUMP.VERSION then
        error("invalid header")
    end
    printer:write(ls, "Header LuaJIT 2.0 BC")
    local flags = bcread_uleb128(ls)
    ls.flags = flags
    printer:write(ls, format("Flags: %s", flags_string(flags)))
    if band(flags, bnot(BCDUMP.F_KNOWN)) ~= 0 then
        error("unknown flags")
    end
    if band(flags, BCDUMP.F_STRIP) == 0 then
        local len = bcread_uleb128(ls)
        bcread_need(ls, len)
        local chunkname = bcread_mem(ls, len)
        printer:write(ls, format("Chunkname: %s", chunkname))
    end
end

local function bcread_bytecode(ls, sizebc)
    bcread_block(ls, (sizebc - 1) * 4)
    printer:write(ls, "Bytecode")
end

local function bcread_uv(ls, sizeuv)
    printer:write(ls, ".. UV ..")
    for i = 1, sizeuv do
        local lo, hi = bcread_byte(ls), bcread_byte(ls)
        if band(hi, 0x80) ~= 0 then
            local constbit = band(hi, 0x40)
            local hix = band(hi, 0x3f)
            printer:write(ls, "upvalue %slocal %d", constbit ~= 0 and "(const) " or "", bor(lo, lshift(hix, 8)))
        else
            printer:write(ls, "upvalue uv index %d", bor(lo, lshift(hi, 8)))
        end
    end
end

local double_new = ffi.typeof('double[1]')
local uint32_new = ffi.typeof('uint32_t[1]')
local int64_new  = ffi.typeof('int64_t[1]')
local uint64_new = ffi.typeof('uint64_t[1]')

local function dword_new_u32(cdata_new, lo, hi)
    local value = cdata_new()
    local char = ffi.cast('uint8_t*', value)
    local u32_lo, u32_hi = uint32_new(lo), uint32_new(hi)
    ffi.copy(char, u32_lo, 4)
    ffi.copy(char + 4, u32_hi, 4)
    return value[0]
end

local function bcread_ktab(ls)
    error("NYI")
end

local function bcread_kgc(ls, sizekgc)
    printer:write(ls, ".. KGC ..")
    for i = 1, sizekgc do
        local tp = bcread_uleb128(ls)
        if tp >= BCDUMP_KGC_STR then
            local len = tp - BCDUMP_KGC_STR
            local str = bcread_mem(ls, len)
            printer:write(ls, "string: %q", str)
        elseif tp == BCDUMP_KGC_TAB then
            bcread_ktab(ls)
        elseif tp ~= BCDUMP_KGC_CHILD then
            local lo0, hi0 = bcread_uleb128(ls), bcread_uleb128(ls)
            if tp == BCDUMP_KGC_COMPLEX then
                local lo1, hi1 = bcread_uleb128(ls), bcread_uleb128(ls)
                local re = dword_new_u32(double_new, lo0, hi0)
                local im = dword_new_u32(double_new, lo1, hi1)
                printer:write(ls, "complex: %g + %gi", re, im)
            else
                local cdata_new = tp == BCDUMP_KGC_I64 and int64_new or uint64_new
                local value = dword_new_u32(cdata_new, lo0, hi0)
                printer:write(ls, "cdata: %s", tostring(value))
            end
        else
            printer:write(ls, "child prototype")
        end
    end
end

local function bcread_knum(ls, sizekn)
    printer:write(ls, ".. KNUM ..")
    for i = 1, sizekn do
        local isnumbit = band(byte(ls), 1)
        local lo = bcread_uleb128_33(ls)
        if isnumbit ~= 0 then
            local hi = bcread_uleb128(ls)
            local value = dword_new_u32(double_new, lo, hi)
            printer:write(ls, "number: %g", value)
        else
            printer:write(ls, "integer: %d", lo)
        end
    end
end

local function bcread_dbg(ls, sizedbg)
    bcread_block(ls, sizedbg)
    printer:write(ls, "Debug informations")
end

local function bcread_proto(ls)
    if ls.n > 0 and byte(ls) == 0 then
        bcread_byte(ls)
        printer:write(ls, "eof")
        return false
    end
    local len = bcread_uleb128(ls)
    local startn = ls.n
    printer:write(ls, "prototype length: %d", len)
    if len == 0 then return false end
    bcread_need(ls, len)

    -- Read prototype header.
    local flags = bcread_byte(ls)
    printer:write(ls, "prototype flags %02x", flags)
    local numparams = bcread_byte(ls)
    printer:write(ls, "parameters number %d", numparams)
    local framesize = bcread_byte(ls)
    printer:write(ls, "framesize %d", framesize)
    local sizeuv = bcread_byte(ls)
    local sizekgc = bcread_uleb128(ls)
    local sizekn = bcread_uleb128(ls)
    local sizebc = bcread_uleb128(ls) + 1
    printer:write(ls, "size uv: %d kgc: %d kn: %d bc: %d", sizeuv, sizekgc, sizekn, sizebc)

    local sizedbg, firstline, numline = 0, 0, 0
    if band(ls.flags, BCDUMP.F_STRIP) == 0 then
        sizedbg = bcread_uleb128(ls)
        printer:write(ls, "debug size %d", sizedbg)
        if sizedbg > 0 then
            firstline = bcread_uleb128(ls)
            numline = bcread_uleb128(ls)
            printer:write(ls, "firstline: %d numline: %d", firstline, numline)
        end
    end

    bcread_bytecode(ls, sizebc)
    bcread_uv(ls, sizeuv)
    bcread_kgc(ls, sizekgc)
    bcread_knum(ls, sizekn)

    if sizedbg > 0 then
        bcread_dbg(ls, sizedbg)
    end

    assert(len == startn - ls.n, "prototype bytecode size mismatch")
    return true
end

local function bcread(s)
    local ls = {data = s, n = #s, p = 1, bytes = {}}
    local err
    if bcread_byte(ls) ~= BCDUMP.HEAD1 then
        return "invalid header beginning char"
    end
    bcread_header(ls)
    repeat
        local found = bcread_proto(ls)
    until not found
    if ls.n > 0 then
        error("spurious bytecode")
    end
end

return { start = bcread }
