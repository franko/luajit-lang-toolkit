--
-- LuaJIT Language Toolkit.
--
-- Copyright (C) 2013-2014 Francesco Abbate. All rights reserved.
--
-- Major portions taken verbatim or adapted from
-- LuaJIT -- a Just-In-Time Compiler for Lua. http://luajit.org/
-- Copyright (C) 2005-2014 Mike Pall. All rights reserved.
--
-- See Copyright Notice in LICENSE
--

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
local BCDUMP_KTAB_NIL, BCDUMP_KTAB_FALSE, BCDUMP_KTAB_TRUE, BCDUMP_KTAB_INT, BCDUMP_KTAB_NUM, BCDUMP_KTAB_STR = 0, 1, 2, 3, 4, 5

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

local function log(out, ls, fmt, ...)
    local n = 1
    local bcount, tlen = 0, 0
    local text = format(fmt, ...)
    repeat
        local alen, a = bytes_row(ls.bytes, n)
        local blen, b = text_fragment(text, n)
        out:write(format("%-24s| %s\n", a, b))
        bcount, tlen = bcount + alen, tlen + blen
        n = n + 1
    until bcount >= #ls.bytes and tlen >= #text
    ls.bytes = {}
end

local function chunkname_strip(s)
    s = gsub(s, "^@", "")
    s = gsub(s, ".+[/\\]", "")
    return s
end

local function proto_new(filename)
    return {
        kgc = {},
        knum = {},
        uv = {},
        lineinfo = {},
        uvinfo = {},
        varinfo = {},
        filename = filename,
        firstline = 0,
        numlines = 0,
    }
end

local function action(obj, method_name, ...)
    local m = obj[method_name]
    if m then m(obj, ...) end
end

local function save_position(ls)
    ls.bytes = {}
    return {p = ls.p, n = ls.n}
end

local function restore_position(ls, save)
    ls.bytes = {}
    ls.p, ls.n = save.p, save.n
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
local function bcline(proto, pc, ins, m, prefix)
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
        local kgc = proto.kgc
        kc = kgc[#kgc - d]
        kc = format(#kc > 40 and '"%.40s"~' or '"%s"', gsub(kc, "%c", ctlsub))
    elseif mc == 9*128 then -- BCMnum
        kc = proto.knum[d+1]
        if op == "TSETM " then kc = kc - 2^52 end
    elseif mc == 12*128 then -- BCMfunc
        local f = proto.kgc[#proto.kgc - d]
        kc = format("%s:%d", f.filename, f.firstline)
    elseif mc == 5*128 then -- BCMuv
        kc = proto.uvinfo[d+1]
    end
    if ma == 5 then -- BCMuv
        local ka = proto.uvinfo[a+1]
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

local function bcread_bytecode(ls, target, sizebc)
    action(target, "enter_bytecode", ls)
    for pc = 1, sizebc - 1 do
        local ins, m = bcread_ins(ls)
        action(target, "ins", ls, pc, ins, m)
    end
end

local function uv_decode(uv)
    if band(uv, 0x8000) ~= 0 then
        local imm = (band(uv, 0x40) ~= 0)
        return band(uv, 0x3fff), true, imm
    else
        return uv, false, false
    end
end

local function bcread_uv(ls, target, sizeuv)
    action(target, "enter_uv", ls)
    for i = 1, sizeuv do
        local uv = bcread_uint16(ls)
        action(target, "uv", ls, i, uv)
    end
end

local double_new = ffi.typeof('double[1]')
local uint32_new = ffi.typeof('uint32_t[1]')
local int64_new  = ffi.typeof('int64_t[1]')
local uint64_new = ffi.typeof('uint64_t[1]')
local complex    = ffi.typeof('complex')

local function dword_new_u32(cdata_new, lo, hi)
    local value = cdata_new()
    local char = ffi.cast('uint8_t*', value)
    local u32_lo, u32_hi = uint32_new(lo), uint32_new(hi)
    ffi.copy(char, u32_lo, 4)
    ffi.copy(char + 4, u32_hi, 4)
    return value[0]
end

local function bcread_ktabk(ls, target)
    local tp = bcread_uleb128(ls)
    if tp >= BCDUMP_KTAB_STR then
        local len = tp - BCDUMP_KTAB_STR
        local str = bcread_mem(ls, len)
        action(target, "ktabk", ls, "string", str)
    elseif tp == BCDUMP_KTAB_INT then
        local n = bcread_uleb128(ls)
        action(target, "ktabk", ls, "int", n)
    elseif tp == BCDUMP_KTAB_NUM then
        local lo = bcread_uleb128(ls)
        local hi = bcread_uleb128(ls)
        local value = dword_new_u32(double_new, lo, hi)
        action(target, "ktabk", ls, "num", value)
    else
        assert(tp <= BCDUMP_KTAB_TRUE)
        action(target, "ktabk", ls, "pri", tp)
    end
end

local function bcread_ktab(ls, target)
    local narray = bcread_uleb128(ls)
    local nhash = bcread_uleb128(ls)
    action(target, "ktab_dim", ls, narray, nhash)
    for i = 1, narray do
        bcread_ktabk(ls, target)
    end
    for i = 1, nhash do
       bcread_ktabk(ls, target)
       bcread_ktabk(ls, target)
    end
    return -1
end

local function bcread_kgc(ls, target, sizekgc)
    action(target, "enter_kgc", ls)
    for i = 1, sizekgc do
        local tp = bcread_uleb128(ls)
        if tp >= BCDUMP_KGC_STR then
            local len = tp - BCDUMP_KGC_STR
            local str = bcread_mem(ls, len)
            action(target, "kgc", ls, i, str)
        elseif tp == BCDUMP_KGC_TAB then
            local value = bcread_ktab(ls, target)
            action(target, "kgc", ls, i, value)
        elseif tp ~= BCDUMP_KGC_CHILD then
            local lo0, hi0 = bcread_uleb128(ls), bcread_uleb128(ls)
            if tp == BCDUMP_KGC_COMPLEX then
                local lo1, hi1 = bcread_uleb128(ls), bcread_uleb128(ls)
                local re = dword_new_u32(double_new, lo0, hi0)
                local im = dword_new_u32(double_new, lo1, hi1)
                action(target, "kgc", ls, i, complex(re, im))
            else
                local cdata_new = tp == BCDUMP_KGC_I64 and int64_new or uint64_new
                local value = dword_new_u32(cdata_new, lo0, hi0)
                action(target, "kgc", ls, i, value)
            end
        else
            action(target, "kgc", ls, i, 0)
        end
    end
end

local function bcread_knum(ls, target, sizekn)
    action(target, "enter_knum", ls)
    for i = 1, sizekn do
        local isnumbit = band(byte(ls), 1)
        local lo = bcread_uleb128_33(ls)
        if isnumbit ~= 0 then
            local hi = bcread_uleb128(ls)
            local value = dword_new_u32(double_new, lo, hi)
            action(target, "knum", ls, i, "num", value)
        else
            action(target, "knum", ls, i, "int", lo)
        end
    end
end

local function bcread_lineinfo(ls, target, firstline, numlines, sizebc, sizedbg)
    if numlines < 256 then
        for pc = 1, sizebc - 1 do
            local line = bcread_byte(ls)
            action(target, "lineinfo", ls, pc, firstline + line)
        end
    elseif numlines < 65536 then
        for pc = 1, sizebc - 1 do
            local line = bcread_uint16(ls)
            action(target, "lineinfo", ls, pc, firstline + line)
        end
    else
        for pc = 1, sizebc - 1 do
            local line = bcread_uint32(ls)
            action(target, "lineinfo", ls, pc, firstline + line)
        end
    end
end

local function bcread_uvinfo(ls, target, sizeuv)
    for i = 1, sizeuv do
        local name = bcread_string(ls)
        action(target, "uvinfo", ls, i, name)
    end
end

local VARNAME = {
  "(for index)", "(for limit)", "(for step)", "(for generator)",
  "(for state)", "(for control)"
}

local function bcread_varinfo(ls, target)
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
        action(target, "varinfo", ls, name, startpc, endpc)
        lastpc = startpc
    end
end

local function bcread_dbg(ls, target, firstline, numlines, sizebc, sizeuv, sizedbg)
    action(target, "enter_debug", ls)
    bcread_lineinfo(ls, target, firstline, numlines, sizebc, sizedbg)
    bcread_uvinfo(ls, target, sizeuv)
    bcread_varinfo(ls, target)
end

-- This function return an object used as target by bcread_* routines in the
-- first pass of bytecode read. The role of this object is to acquire
-- informations about kgc, knum, uv, jump targets etc.
-- The informations are stored in the "proto" object and used by the "printer"
-- object in the second pass.
local function proto_info_target(target)
    local proto = target.proto
    local function knum(_, ls, i, tag, value)
        proto.knum[i] = value
    end
    local function kgc(_, ls, i, value)
        if value == 0 then
            value = table.remove(target.childs)
        end
        proto.kgc[i] = value
    end
    local function uv(_, ls, i, value)
        proto.uv[i] = value
    end
    local function lineinfo(_, ls, pc, line)
        proto.lineinfo[pc] = line
    end
    local function uvinfo(_, ls, i, name)
        proto.uvinfo[i] = name
    end
    local function varinfo(_, ls, name, startpc, endpc)
        proto.varinfo[#proto.varinfo + 1] = {name, startpc, endpc}
    end
    local function enter_bytecode()
        proto.target = {}
    end
    local function ins(_, ls, pc, ins, m)
        if band(m, 15*128) == 13*128 then proto.target[pc+shr(ins, 16)-0x7fff] = true end
    end
    return {
        knum = knum, kgc = kgc, uv = uv,
        lineinfo = lineinfo, uvinfo = uvinfo, varinfo = varinfo,
        enter_bytecode = enter_bytecode, ins = ins,
    }
end

local function bcread_proto(ls, target)
    if ls.n > 0 and byte(ls) == 0 then
        bcread_byte(ls)
        action(target, "eof", ls)
        return nil
    end
    action(target, "enter_proto", ls)
    local proto = proto_new(chunkname_strip(target.chunkname))
    target.proto = proto
    local len = bcread_uleb128(ls)
    local startn = ls.n
    action(target, "proto_len", ls, len)
    if len == 0 then return nil end
    bcread_need(ls, len)

    -- Read prototype header.
    local flags = bcread_byte(ls)
    action(target, "proto_flags", ls, flags)
    local numparams = bcread_byte(ls)
    action(target, "proto_numparams", ls, numparams)
    local framesize = bcread_byte(ls)
    action(target, "proto_framesize", ls, framesize)
    local sizeuv = bcread_byte(ls)
    local sizekgc = bcread_uleb128(ls)
    local sizekn = bcread_uleb128(ls)
    local sizebc = bcread_uleb128(ls) + 1
    action(target, "proto_sizes", ls, sizeuv, sizekgc, sizekn, sizebc)

    local sizedbg, firstline, numlines = 0, 0, 0
    if band(ls.flags, BCDUMP.F_STRIP) == 0 then
        sizedbg = bcread_uleb128(ls)
        action(target, "proto_debug_size", ls, sizedbg)
        if sizedbg > 0 then
            firstline = bcread_uleb128(ls)
            numlines = bcread_uleb128(ls)
            proto.firstline, proto.numlines = firstline, numlines
            action(target, "proto_lines", ls, firstline, numlines)
        end
    end

    local info = proto_info_target(target)
    if info then
        local save = save_position(ls)
        bcread_bytecode(ls, info, sizebc)
        bcread_uv(ls, info, sizeuv)
        bcread_kgc(ls, info, sizekgc)
        bcread_knum(ls, info, sizekn)
        if sizedbg > 0 then
            bcread_dbg(ls, info, firstline, numlines, sizebc, sizeuv, sizedbg)
        end
        restore_position(ls, save)
    end

    bcread_bytecode(ls, target, sizebc)
    bcread_uv(ls, target, sizeuv)
    bcread_kgc(ls, target, sizekgc)
    bcread_knum(ls, target, sizekn)
    if sizedbg > 0 then
        bcread_dbg(ls, target, firstline, numlines, sizebc, sizeuv, sizedbg)
    end

    assert(len == startn - ls.n, "prototype bytecode size mismatch")
    return target.proto
end

local function bcread_header(ls, target)
    if bcread_byte(ls) ~= BCDUMP.HEAD2 or bcread_byte(ls) ~= BCDUMP.HEAD3 or bcread_byte(ls) ~= BCDUMP.VERSION then
        error("invalid header")
    end
    action(target, "header", ls)
    local flags = bcread_uleb128(ls)
    ls.flags = flags
    action(target, "flags", ls, flags)
    if band(flags, bnot(BCDUMP.F_KNOWN)) ~= 0 then
        error("unknown flags")
    end
    if band(flags, BCDUMP.F_STRIP) == 0 then
        local len = bcread_uleb128(ls)
        bcread_need(ls, len)
        target.chunkname = bcread_mem(ls, len)
        action(target, "set_chunkname", ls, target.chunkname)
    end
end

-- The "printer" object is used to pretty-print on the screen the bytecode's
-- hex dump side by side with the decoded meaning of each chunk of bytes.
-- The routines bcread_* reads the bytecode and calls an appropriate "printer"
-- method with the decoded informations. In turns the "printer" method write on
-- the screen the bytes and the informations.
-- The "printer" object assume that a "proto" field is available with some
-- prototype's informations. The required informations includes kgc, knum, uv,
-- debug name and line numbers.

local Printer = { }

function Printer:set_chunkname(ls, chunkname)
    log(self.out, ls, format("Chunkname: %s", chunkname))
end

function Printer:enter_proto(ls)
    log(self.out, ls, ".. prototype ..")
end

function Printer:header(ls) log(self.out, ls, "Header LuaJIT 2.0 BC") end
function Printer:flags(ls, flags) log(self.out, ls, format("Flags: %s", flags_string(flags))) end
function Printer:enter_kgc(ls) log(self.out, ls, ".. kgc ..") end
function Printer:enter_knum(ls) log(self.out, ls, ".. knum ..") end
function Printer:enter_bytecode(ls) log(self.out, ls, ".. bytecode ..") end
function Printer:enter_uv(ls) log(self.out, ls, ".. uv ..") end
function Printer:enter_debug(ls) log(self.out, ls, ".. debug ..") end
function Printer:eof(ls) log(self.out, ls, "eof") end
function Printer:proto_flags(ls, flags) log(self.out, ls, "prototype flags %s", proto_flags_string(flags)) end
function Printer:proto_len(ls, len) log(self.out, ls, "prototype length %d", len) end
function Printer:proto_numparams(ls, numparams) log(self.out, ls, "parameters number %d", numparams) end
function Printer:proto_framesize(ls, framesize) log(self.out, ls, "framesize %d", framesize) end
function Printer:proto_sizes(ls, sizeuv, sizekgc, sizekn, sizebc) log(self.out, ls, "size uv: %d kgc: %d kn: %d bc: %d", sizeuv, sizekgc, sizekn, sizebc) end
function Printer:proto_debug_size(ls, sizedbg) log(self.out, ls, "debug size %d", sizedbg) end

function Printer:proto_lines(ls, firstline, numlines)
    log(self.out, ls, "firstline: %d numline: %d", firstline, numlines)
end

function Printer:ins(ls, pc, ins, m)
    local s = bcline(self.proto, pc, ins, m, self.proto.target[pc] and "=>")
    log(self.out, ls, "%s", s)
end

function Printer:knum(ls, i, tag, num)
    log(self.out, ls, "knum %s: %g", tag, num)
end

function Printer:kgc(ls, i, value)
    local str
    if type(value) == "string" then
        str = format("%q", value)
    elseif value == 0 then
        local pt = self.proto.kgc[i]
        str = format("<function: %s:%d>", pt.filename, pt.firstline)
    else
        str = tostring(value)
    end
    log(self.out, ls, "kgc: %s", str)
end

function Printer:ktab_dim(ls, narray, nhash)
    log(self.out, ls, "ktab narray: %d nhash: %d", narray, nhash)
end

function Printer:ktabk(ls, tag, value)
    local ps = {"nil", "false", "true"}
    local s = tag == "string" and format("%q", value) or (tag == "pri" and ps[value] or tostring(value))
    log(self.out, ls, "ktabk %s: %s", tag, s)
end

function Printer:uv(ls, i, value)
    local uv, islocal, imm = uv_decode(value)
    if islocal then
        log(self.out, ls, "upvalue %slocal %d", imm and "(const) " or "", uv)
    else
        log(self.out, ls, "upvalue upper %d", uv)
    end
end

function Printer:lineinfo(ls, pc, line)
    log(self.out, ls, "pc%03d: line %d", pc, line)
end

function Printer:uvinfo(ls, i, name)
    log(self.out, ls, "uv%d: name: %s", i - 1, name)
end

function Printer:varinfo(ls, name, startpc, endpc)
    log(self.out, ls, "var: %s pc: %d - %d", name, startpc, endpc)
end

-- The BCList object is used to print the bytecode instructions as
-- "luajit -bl" does.
local BCList = { }

function BCList:enter_bytecode()
    local pt = self.proto
    self.out:write(format("-- BYTECODE -- %s:%d-%d\n", pt.filename, pt.firstline, pt.firstline + pt.numlines))
end

function BCList:ins(ls, pc, ins, m)
    local s = bcline(self.proto, pc, ins, m, self.proto.target[pc] and "=>")
    self.out:write(s)
    self.out:write("\n")
end

function BCList:enter_uv() self.out:write("\n") end

local function printer_new(output, class, chunkname)
    local p = { out = output, childs = {}, chunkname = chunkname }
    return setmetatable(p, { __index = class })
end

local function bcread(s, output, chunkname, hexdump)
    local ls = {data = s, n = #s, p = 1, bytes = {}}
    local printer = printer_new(output, hexdump and Printer or BCList, chunkname)
    if bcread_byte(ls) ~= BCDUMP.HEAD1 then
        return "invalid header beginning char"
    end
    bcread_header(ls, printer)
    repeat
        local pt = bcread_proto(ls, printer)
        printer.childs[#printer.childs + 1] = pt
    until not pt
    if ls.n > 0 then
        error("spurious bytecode")
    end
end

return { dump = bcread }
