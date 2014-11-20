local ffi = require("ffi")

local band, bor, shl, shr, bnot = bit.band, bit.bor, bit.lshift, bit.rshift, bit.bnot
local strsub, strbyte, strchar, format, gsub = string.sub, string.byte, string.char, string.format, string.gsub

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

local BCM_REF = {
    'none', 'dst', 'base', 'var', 'rbase', 'uv',  -- Mode A must be <= 7
    'lit', 'lits', 'pri', 'num', 'str', 'tab', 'func', 'jump', 'cdata'
}

local BCDEF_TAB = {
    {'ISLT', 'var', 'none', 'var', 'lt'},
    {'ISGE', 'var', 'none', 'var', 'lt'},
    {'ISLE', 'var', 'none', 'var', 'le'},
    {'ISGT', 'var', 'none', 'var', 'le'},

    {'ISEQV', 'var', 'none', 'var', 'eq'},
    {'ISNEV', 'var', 'none', 'var', 'eq'},
    {'ISEQS', 'var', 'none', 'str', 'eq'},
    {'ISNES', 'var', 'none', 'str', 'eq'},
    {'ISEQN', 'var', 'none', 'num', 'eq'},
    {'ISNEN', 'var', 'none', 'num', 'eq'},
    {'ISEQP', 'var', 'none', 'pri', 'eq'},
    {'ISNEP', 'var', 'none', 'pri', 'eq'},

    -- Unary test and copy ops.
    {'ISTC', 'dst', 'none', 'var', 'none'},
    {'ISFC', 'dst', 'none', 'var', 'none'},
    {'IST', 'none', 'none', 'var', 'none'},
    {'ISF', 'none', 'none', 'var', 'none'},

    -- Unary ops.
    {'MOV', 'dst', 'none', 'var', 'none'},
    {'NOT', 'dst', 'none', 'var', 'none'},
    {'UNM', 'dst', 'none', 'var', 'unm'},
    {'LEN', 'dst', 'none', 'var', 'len'},

    -- Binary ops. ORDER OPR. VV last, POW must be next.
    {'ADDVN', 'dst', 'var', 'num', 'add'},
    {'SUBVN', 'dst', 'var', 'num', 'sub'},
    {'MULVN', 'dst', 'var', 'num', 'mul'},
    {'DIVVN', 'dst', 'var', 'num', 'div'},
    {'MODVN', 'dst', 'var', 'num', 'mod'},

    {'ADDNV', 'dst', 'var', 'num', 'add'},
    {'SUBNV', 'dst', 'var', 'num', 'sub'},
    {'MULNV', 'dst', 'var', 'num', 'mul'},
    {'DIVNV', 'dst', 'var', 'num', 'div'},
    {'MODNV', 'dst', 'var', 'num', 'mod'},

    {'ADDVV', 'dst', 'var', 'var', 'add'},
    {'SUBVV', 'dst', 'var', 'var', 'sub'},
    {'MULVV', 'dst', 'var', 'var', 'mul'},
    {'DIVVV', 'dst', 'var', 'var', 'div'},
    {'MODVV', 'dst', 'var', 'var', 'mod'},

    {'POW', 'dst', 'var', 'var', 'pow'},
    {'CAT', 'dst', 'rbase', 'rbase', 'concat'},

    -- Constant ops.
    {'KSTR', 'dst', 'none', 'str', 'none'},
    {'KCDATA', 'dst', 'none', 'cdata', 'none'},
    {'KSHORT', 'dst', 'none', 'lits', 'none'},
    {'KNUM', 'dst', 'none', 'num', 'none'},
    {'KPRI', 'dst', 'none', 'pri', 'none'},
    {'KNIL', 'base', 'none', 'base', 'none'},

    -- Upvalue and function ops.
    {'UGET', 'dst', 'none', 'uv', 'none'},
    {'USETV', 'uv', 'none', 'var', 'none'},
    {'USETS', 'uv', 'none', 'str', 'none'},
    {'USETN', 'uv', 'none', 'num', 'none'},
    {'USETP', 'uv', 'none', 'pri', 'none'},
    {'UCLO', 'rbase', 'none', 'jump', 'none'},
    {'FNEW', 'dst', 'none', 'func', 'gc'},

    -- Table ops.
    {'TNEW', 'dst', 'none', 'lit', 'gc'},
    {'TDUP', 'dst', 'none', 'tab', 'gc'},
    {'GGET', 'dst', 'none', 'str', 'index'},
    {'GSET', 'var', 'none', 'str', 'newindex'},
    {'TGETV', 'dst', 'var', 'var', 'index'},
    {'TGETS', 'dst', 'var', 'str', 'index'},
    {'TGETB', 'dst', 'var', 'lit', 'index'},
    {'TSETV', 'var', 'var', 'var', 'newindex'},
    {'TSETS', 'var', 'var', 'str', 'newindex'},
    {'TSETB', 'var', 'var', 'lit', 'newindex'},
    {'TSETM', 'base', 'none', 'num', 'newindex'},

    -- Calls and vararg handling. T = tail call.
    {'CALLM', 'base', 'lit', 'lit', 'call'},
    {'CALL', 'base', 'lit', 'lit', 'call'},
    {'CALLMT', 'base', 'none', 'lit', 'call'},
    {'CALLT', 'base', 'none', 'lit', 'call'},
    {'ITERC', 'base', 'lit', 'lit', 'call'},
    {'ITERN', 'base', 'lit', 'lit', 'call'},
    {'VARG', 'base', 'lit', 'lit', 'none'},
    {'ISNEXT', 'base', 'none', 'jump', 'none'},

    -- Returns.
    {'RETM', 'base', 'none', 'lit', 'none'},
    {'RET', 'rbase', 'none', 'lit', 'none'},
    {'RET0', 'rbase', 'none', 'lit', 'none'},
    {'RET1', 'rbase', 'none', 'lit', 'none'},

    -- Loops and branches. I/J = interp/JIT, I/C/L = init/call/loop.
    {'FORI', 'base', 'none', 'jump', 'none'},
    {'JFORI', 'base', 'none', 'jump', 'none'},

    {'FORL', 'base', 'none', 'jump', 'none'},
    {'IFORL', 'base', 'none', 'jump', 'none'},
    {'JFORL', 'base', 'none', 'lit', 'none'},

    {'ITERL', 'base', 'none', 'jump', 'none'},
    {'IITERL', 'base', 'none', 'jump', 'none'},
    {'JITERL', 'base', 'none', 'lit', 'none'},

    {'LOOP', 'rbase', 'none', 'jump', 'none'},
    {'ILOOP', 'rbase', 'none', 'jump', 'none'},
    {'JLOOP', 'rbase', 'none', 'lit', 'none'},

    {'JMP', 'rbase', 'none', 'jump', 'none'},

    -- Function headers. I/J = interp/JIT, F/V/C = fixarg/vararg/C func.
    {'FUNCF', 'rbase', 'none', 'none', 'none'},
    {'IFUNCF', 'rbase', 'none', 'none', 'none'},
    {'JFUNCF', 'rbase', 'none', 'lit', 'none'},
    {'FUNCV', 'rbase', 'none', 'none', 'none'},
    {'IFUNCV', 'rbase', 'none', 'none', 'none'},
    {'JFUNCV', 'rbase', 'none', 'lit', 'none'},
    {'FUNCC', 'rbase', 'none', 'none', 'none'},
    {'FUNCCW', 'rbase',  'none', 'none', 'none'},
}

local BC, BCMODE = {}, {}

local function BCM(name)
    for i = 1, #BCM_REF do
        if BCM_REF[i] == name then return i - 1 end
    end
end

local function BCDEF_EVAL()
    for i = 1, #BCDEF_TAB do
        local li = BCDEF_TAB[i]
        local name, ma, mb, mc = li[1], BCM(li[2]), BCM(li[3]), BCM(li[4])
        BC[i-1] = name
        BCMODE[i-1] = bor(ma, shl(mb, 3), shl(mc, 7))
    end
end

BCDEF_EVAL()

local PROTO_REF = {
    PROTO_CHILD  = 0x01,    -- Has child prototypes.
    PROTO_VARARG = 0x02,    -- Vararg function.
    PROTO_FFI    = 0x04,    -- Uses BC_KCDATA for FFI datatypes.
    PROTO_NOJIT  = 0x08,    -- JIT disabled for this function.
    PROTO_ILOOP  = 0x10,    -- Patched bytecode with ILOOP etc.
    -- Only used during parsing.
    PROTO_HAS_RETURN   = 0x20,    -- Already emitted a return.
    PROTO_FIXUP_RETURN = 0x40,    -- Need to fixup emitted returns.
}

local function proto_flags_string(flags)
    local t = {}
    for name, bit in pairs(PROTO_REF) do
        if band(flags, bit) ~= 0 then t[#t+1] = name end
    end
    return #t > 0 and table.concat(t, "|") or "None"
end

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
    return strbyte(ls.data, p, p)
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

local function bcread_uint16(ls)
    local a, b = strbyte(ls.data, ls.p, ls.p + 1)
    bcread_consume(ls, 2)
    ls.p = ls.p + 2
    return bor(shl(b, 8), a)
end

local function bcread_uint32(ls)
    local a, b, c, d = strbyte(ls.data, ls.p, ls.p + 3)
    bcread_consume(ls, 4)
    ls.p = ls.p + 4
    return bor(shl(d, 24), shl(c, 16), shl(b, 8), a)
end

local function bcread_string(ls)
    local p = ls.p
    while byte(ls, p) ~= 0 and ls.n > 0 do
        p = p + 1
    end
    assert(byte(ls, p) == 0 and p > ls.p, "corrupted bytecode")
    local s = strsub(ls.data, ls.p, p - 1)
    local len = p - ls.p + 1
    bcread_consume(ls, len)
    ls.p = p + 1
    return s
end

local function bcread_uleb128(ls)
    local v = bcread_byte(ls)
    if v >= 0x80 then
        local sh = 0
        v = band(v, 0x7f)
        repeat
            local b = bcread_byte(ls)
            v = bor(v, shl(band(b, 0x7f), sh + 7))
            sh = sh + 7
        until b < 0x80
    end
    return v
end

-- Read top 32 bits of 33 bit ULEB128 value from buffer.
local function bcread_uleb128_33(ls)
    local v = shr(bcread_byte(ls), 1)
    if v >= 0x40 then
        local sh = -1
        v = band(v, 0x3f)
        repeat
            local b = bcread_byte(ls)
            v = bor(v, shl(band(b, 0x7f), sh + 7))
            sh = sh + 7
        until b < 0x80
    end
    return v
end

local function bcread_mem(ls, len)
    local s = strsub(ls.data, ls.p, ls.p + len - 1)
    bcread_consume(ls, len)
    ls.p = ls.p + len
    return s
end

local bcread_block = bcread_mem


local function ctlsub(c)
    if c == "\n" then return "\\n"
elseif c == "\r" then return "\\r"
    elseif c == "\t" then return "\\t"
    else return format("\\%03d", byte(c))
    end
end

local function bcread_ins(ls)
    local ins = bcread_uint32(ls)
    local op = band(ins, 0xff)
    return ins, BCMODE[op]
end

-- Return one bytecode line.
local function bcline(ls, pc, prefix)
    local ins, m = bcread_ins(ls)
    if not ins then return end
    local ma, mb, mc = band(m, 7), band(m, 15*8), band(m, 15*128)
    local a = band(shr(ins, 8), 0xff)
    local op = BC[band(ins, 0xff)]
    local s = format("%04d %s %-6s %3s ", pc, prefix or "  ", op, ma == 0 and "" or a)
    local d = shr(ins, 16)
    if mc == 13*128 then -- BCMjump
        return format("%s=> %04d", s, pc+d-0x7fff)
    end
    if mb ~= 0 then
        d = band(d, 0xff)
    elseif mc == 0 then
        return s
    end
    local kc
    if mc == 10*128 then -- BCMstr
        kc = format("<kgc string: %d>", d)
    elseif mc == 9*128 then -- BCMnum
        kc = format("<knum: %d>", d)
        if op == "TSETM " then kc = kc - 2^52 end
    elseif mc == 12*128 then -- BCMfunc
        kc = format("<function: %d>", d)
    elseif mc == 5*128 then -- BCMuv
        kc = format("<uv: %d>", d)
    end
    if ma == 5 then -- BCMuv
        local ka = format("<uv: %d>", a)
        if kc then kc = ka.." ; "..kc else kc = ka end
    end
    if mb ~= 0 then
        local b = shr(ins, 24)
        if kc then return format("%s%3d %3d  ; %s", s, b, d, kc) end
        return format("%s%3d %3d", s, b, d)
    end
    if kc then return format("%s%3d      ; %s", s, d, kc) end
    if mc == 7*128 and d > 32767 then d = d - 65536 end -- BCMlits
    return format("%s%3d", s, d)
end

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
    printer:write(ls, ".. bytecode ..")
    for pc = 1, sizebc - 1 do
        local ins = bcline(ls, pc)
        printer:write(ls, ins)
    end
end

local function bcread_uv(ls, sizeuv)
    printer:write(ls, ".. UV ..")
    for i = 1, sizeuv do
        local lo, hi = bcread_byte(ls), bcread_byte(ls)
        if band(hi, 0x80) ~= 0 then
            local constbit = band(hi, 0x40)
            local hix = band(hi, 0x3f)
            printer:write(ls, "upvalue %slocal %d", constbit ~= 0 and "(const) " or "", bor(lo, shl(hix, 8)))
        else
            printer:write(ls, "upvalue uv index %d", bor(lo, shl(hi, 8)))
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

local function bcread_lineinfo(ls, firstline, numline, sizebc, sizedbg)
    if numline < 256 then
        for pc = 1, sizebc - 1 do
            local line = bcread_byte(ls)
            printer:write(ls, "pc%03d: line %d", pc, firstline + line)
        end
    elseif numline < 65536 then
        for pc = 1, sizebc - 1 do
            local line = bcread_uint16(ls)
            printer:write(ls, "pc%03d: line %d", pc, firstline + line)
        end
    else
        for pc = 1, sizebc - 1 do
            local line = bcread_uint32(ls)
            printer:write(ls, "pc%03d: line %d", pc, firstline + line)
        end
    end
end

local function bcread_uvinfo(ls, sizeuv)
    for i = 1, sizeuv do
        local name = bcread_string(ls)
        printer:write(ls, "uv%d: name: %s", i - 1, name)
    end
end

local VARNAME = {
  "(for index)", "(for limit)", "(for step)", "(for generator)",
  "(for state)", "(for control)"
}

local function bcread_varinfo(ls)
    local lastpc = 0
    while true do
        local vn = byte(ls)
        local name
        if vn < #VARNAME + 1 then
            bcread_byte(ls)
            if vn == 0 then break end
            name = VARNAME[vn]
        else
            name = bcread_string(ls)
        end
        local startpc = lastpc + bcread_uleb128(ls)
        local endpc = startpc + bcread_uleb128(ls)
        printer:write(ls, "var: %s pc: %d - %d", name, startpc, endpc)
        lastpc = startpc
    end
end

local function bcread_dbg(ls, firstline, numline, sizebc, sizeuv, sizedbg)
    printer:write(ls, ".. debug info ..")
    bcread_lineinfo(ls, firstline, numline, sizebc, sizedbg)
    bcread_uvinfo(ls, sizeuv)
    bcread_varinfo(ls)
end

local function bcread_proto(ls)
    if ls.n > 0 and byte(ls) == 0 then
        bcread_byte(ls)
        printer:write(ls, "eof")
        return false
    end
    printer:write(ls, "")
    printer:write(ls, ".. Prototype ..")
    local len = bcread_uleb128(ls)
    local startn = ls.n
    printer:write(ls, "prototype length: %d", len)
    if len == 0 then return false end
    bcread_need(ls, len)

    -- Read prototype header.
    local flags = bcread_byte(ls)
    printer:write(ls, "prototype flags %s", proto_flags_string(flags))
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
        bcread_dbg(ls, firstline, numline, sizebc, sizeuv, sizedbg)
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
