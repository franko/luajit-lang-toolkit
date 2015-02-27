--[=[
 dump   = header proto+ 0U
 header = ESC 'L' 'J' versionB flagsU [namelenU nameB*]
 proto  = lengthU pdata
 pdata  = phead bcinsW* uvdataH* kgc* knum* [debugB*]
 phead  = flagsB numparamsB framesizeB numuvB numkgcU numknU numbcU
          [debuglenU [firstlineU numlineU]]
 kgc    = kgctypeU { ktab | (loU hiU) | (rloU rhiU iloU ihiU) | strB* }
 knum   = intU0 | (loU1 hiU)
 ktab   = narrayU nhashU karray* khash*
 karray = ktabk
 khash  = ktabk ktabk
 ktabk  = ktabtypeU { intU | (loU hiU) | strB* }
 uvdata = register index with high bit set if local to outer
 debug  = lninfoV* uvals vars '\0'
 uvals  = nameB* '\0'
 vars   = nameB* '\0' startU endU

 B = 8 bit,
 H = 16 bit,
 W = 32 bit,
 V = B, H or W,
 U = ULEB128 of W, U0/U1 = ULEB128 of W+1,
]=]

local bit  = require 'bit'
local ffi  = require 'ffi'
local jit  = require 'jit'

local band, bor, shl, shr, bnot = bit.band, bit.bor, bit.lshift, bit.rshift, bit.bnot

local jit_v21 = jit.version_num >= 20100

local typeof = getmetatable

local function enum(t)
    for i=0,#t do t[t[i]] = i end
    return t
end

-- forward declarations
local Buf, Ins, Proto, Dump, KNum, KObj

local MAX_REG = 200
local MAX_UVS = 60

local function enum_mode(t)
    local re, rm = {}, {}
    local nskip = 0
    for i = 1, #t do
        local v = t[i]
        if v[3] == false then
            nskip = nskip + 1
        else
            local idx = i - nskip - 1
            rm[idx ] = v[2]
            re[v[1]] = idx
        end
    end
    return re, rm
end

local BC_ABC = 0
local BC_AD  = 1
local BC_AJ  = 2

local BC, BC_MODE = enum_mode {
    { 'ISLT'  , BC_AD  },
    { 'ISGE'  , BC_AD  },
    { 'ISLE'  , BC_AD  },
    { 'ISGT'  , BC_AD  },
    { 'ISEQV' , BC_AD  },
    { 'ISNEV' , BC_AD  },
    { 'ISEQS' , BC_AD  },
    { 'ISNES' , BC_AD  },
    { 'ISEQN' , BC_AD  },
    { 'ISNEN' , BC_AD  },
    { 'ISEQP' , BC_AD  },
    { 'ISNEP' , BC_AD  },
    { 'ISTC'  , BC_AD  },
    { 'ISFC'  , BC_AD  },
    { 'IST'   , BC_AD  },
    { 'ISF'   , BC_AD  },
    { 'ISTYPE', BC_AD, jit_v21 },
    { 'ISNUM' , BC_AD, jit_v21 },
    { 'MOV'   , BC_AD  },
    { 'NOT'   , BC_AD  },
    { 'UNM'   , BC_AD  },
    { 'LEN'   , BC_AD  },
    { 'ADDVN' , BC_ABC },
    { 'SUBVN' , BC_ABC },
    { 'MULVN' , BC_ABC },
    { 'DIVVN' , BC_ABC },
    { 'MODVN' , BC_ABC },
    { 'ADDNV' , BC_ABC },
    { 'SUBNV' , BC_ABC },
    { 'MULNV' , BC_ABC },
    { 'DIVNV' , BC_ABC },
    { 'MODNV' , BC_ABC },
    { 'ADDVV' , BC_ABC },
    { 'SUBVV' , BC_ABC },
    { 'MULVV' , BC_ABC },
    { 'DIVVV' , BC_ABC },
    { 'MODVV' , BC_ABC },
    { 'POW'   , BC_ABC },
    { 'CAT'   , BC_ABC },
    { 'KSTR'  , BC_AD  },
    { 'KCDATA', BC_AD  },
    { 'KSHORT', BC_AD  },
    { 'KNUM'  , BC_AD  },
    { 'KPRI'  , BC_AD  },
    { 'KNIL'  , BC_AD  },
    { 'UGET'  , BC_AD  },
    { 'USETV' , BC_AD  },
    { 'USETS' , BC_AD  },
    { 'USETN' , BC_AD  },
    { 'USETP' , BC_AD  },
    { 'UCLO'  , BC_AJ  },
    { 'FNEW'  , BC_AD  },
    { 'TNEW'  , BC_AD  },
    { 'TDUP'  , BC_AD  },
    { 'GGET'  , BC_AD  },
    { 'GSET'  , BC_AD  },
    { 'TGETV' , BC_ABC },
    { 'TGETS' , BC_ABC },
    { 'TGETB' , BC_ABC },
    { 'TGETR' , BC_ABC, jit_v21 },
    { 'TSETV' , BC_ABC },
    { 'TSETS' , BC_ABC },
    { 'TSETB' , BC_ABC },
    { 'TSETM' , BC_AD  },
    { 'TSETR' , BC_ABC, jit_v21 },
    { 'CALLM' , BC_ABC },
    { 'CALL'  , BC_ABC },
    { 'CALLMT', BC_AD  },
    { 'CALLT' , BC_AD  },
    { 'ITERC' , BC_ABC },
    { 'ITERN' , BC_ABC },
    { 'VARG'  , BC_ABC },
    { 'ISNEXT', BC_AJ  },
    { 'RETM'  , BC_AD  },
    { 'RET'   , BC_AD  },
    { 'RET0'  , BC_AD  },
    { 'RET1'  , BC_AD  },
    { 'FORI'  , BC_AJ  },
    { 'JFORI' , BC_AJ  },
    { 'FORL'  , BC_AJ  },
    { 'IFORL' , BC_AJ  },
    { 'JFORL' , BC_AD  },
    { 'ITERL' , BC_AJ  },
    { 'IITERL', BC_AJ  },
    { 'JITERL', BC_AD  },
    { 'LOOP'  , BC_AJ  },
    { 'ILOOP' , BC_AJ  },
    { 'JLOOP' , BC_AD  },
    { 'JMP'   , BC_AJ  },
    { 'FUNCF' , BC_AD  },
    { 'IFUNCF', BC_AD  },
    { 'JFUNCF', BC_AD  },
    { 'FUNCV' , BC_AD  },
    { 'IFUNCV', BC_AD  },
    { 'JFUNCV', BC_AD  },
    { 'FUNCC' , BC_AD  },
    { 'FUNCCW', BC_AD  },
}

local VKNIL   = 0
local VKFALSE = 1
local VKTRUE  = 2

local NO_JMP = bnot(0)

-- Type codes for the GC constants of a prototype. Plus length for strings.
local KOBJ = enum {
    [0] = "CHILD", "TAB", "I64", "U64", "COMPLEX", "STR",
}

-- Type codes for the keys/values of a constant table.
local KTAB = enum {
    [0] = "NIL", "FALSE", "TRUE", "INT", "NUM", "STR",
}

local FOR_IDX   = "(for index)";
local FOR_STOP  = "(for limit)";
local FOR_STEP  = "(for step)";
local FOR_GEN   = "(for generator)";
local FOR_STATE = "(for state)";
local FOR_CTL   = "(for control)";

local function num_is_int32(x)
    return x % 1 == 0 and x >= -2^31 and x < 2^31
end

Buf = {}
Buf.new = function(size)
    size = size or 2048
    local self = {
        data = ffi.new('char[?]', size),
        size = size,
        offs = 0,
    }
    return setmetatable(self, Buf)
end

Buf.__index = {}
Buf.__index.need = function(self, size)
    local need_size = self.offs + size
    if self.size <= need_size then
        local prev_size = self.size
        while self.size <= need_size do
            self.size = self.size * 2
        end
        local new_data = ffi.new('char[?]', self.size)
        ffi.copy(new_data, self.data, prev_size)
        self.data = new_data
    end
end
Buf.__index.put = function(self, v)
    self:need(1)
    local offs = self.offs
    self.data[offs] = v
    self.offs = offs + 1
    return offs
end
Buf.__index.put_uint8 = Buf.__index.put

Buf.__index.put_uint16 = function(self, v)
    self:need(2)
    local offs = self.offs
    local dptr = self.data + offs
    dptr[0] = v
    v = shr(v, 8)
    dptr[1] = v
    self.offs = offs + 2
    return offs
end

Buf.__index.put_uint32 = function(self, v)
    self:need(4)
    local offs = self.offs
    local dptr = self.data + offs

    dptr[0] = v
    v = shr(v, 8)
    dptr[1] = v
    v = shr(v, 8)
    dptr[2] = v
    v = shr(v, 8)
    dptr[3] = v

    self.offs = offs + 4
    return offs
end

Buf.__index.put_uleb128 = function(self,  v)
    local offs = self.offs
    local b = band(v, 0x7f)
    v = shr(v, 7)
    if v ~= 0 then b = bor(b, 0x80) end
    self:put(b)
    while v > 0 do
        b = band(v, 0x7f)
        v = shr(v, 7)
        if v ~= 0 then b = bor(b, 0x80) end
        self:put(b)
    end
    return offs
end

-- Write a 32 bit unsigned integer + 1 bit given by "numbit".
-- This last argument should be either 0 or 1.
Buf.__index.put_uleb128_33 = function(self,  v, numbit)
    local offs = self.offs
    local b = bor(shl(band(v, 0x3f), 1), numbit)
    v = shr(v, 6)
    if v ~= 0 then b = bor(b, 0x80) end
    self:put(b)
    while v > 0 do
        b = band(v, 0x7f)
        v = shr(v, 7)
        if v ~= 0 then b = bor(b, 0x80) end
        self:put(b)
    end
    return offs
end

Buf.__index.put_bytes = function(self, v)
    local offs = self.offs
    self:need(#v)
    ffi.copy(self.data + offs, v)
    self.offs = offs + #v
    return offs
end
Buf.__index.pack = function(self)
    return ffi.string(self.data, self.offs)
end

local double_new = ffi.typeof('double[1]')
local uint32_new = ffi.typeof('uint32_t[1]')
local int64_new  = ffi.typeof('int64_t[1]')
local uint64_new = ffi.typeof('uint64_t[1]')

local function dword_get_u32(cdata_new, v)
    local p = cdata_new(v)
    local char = ffi.cast('uint8_t*', p)
    local lo, hi = uint32_new(0), uint32_new(0)
    ffi.copy(lo, char, 4)
    ffi.copy(hi, char + 4, 4)
    return lo[0], hi[0]
end

Buf.__index.put_number = function(self, v)
    local offs = self.offs
    if num_is_int32(v) then
        self:put_uleb128_33(v, 0)
    else
        local lo, hi = dword_get_u32(double_new, v)
        self:put_uleb128_33(lo, 1)
        self:put_uleb128(hi)
    end
    return offs
end

Ins = {}
Ins.__index = {}
function Ins.new(op, a, b, c)
    return setmetatable({
        op;
        a or 0;
        b or 0;
        c or 0;
    }, Ins)
end
function Ins.__index:rewrite(op, a, b, c)
    self[1], self[2], self[3], self[4] = op, a or 0, b or 0, c or 0
end
function Ins.__index:write(buf)
    local op, a = self[1], self[2]
    buf:put(op)
    buf:put(a)
    local mode = BC_MODE[op]
    if mode == BC_ABC then
        local b, c = self[3], self[4]
        buf:put(c)
        buf:put(b)
    elseif mode == BC_AD then
        local d = self[3]
        buf:put_uint16(d)
    elseif mode == BC_AJ then
        local j = self[3]
        buf:put_uint16(j + 0x8000)
    else
        error("bad instruction ["..tostring(op).."] (op mode unknown)")
    end
end
local function hsize2hbits(s)
    if s <= 0 then
        return 0
    elseif s == 1 then
        return 1
    end
    s = s - 1
    local c = 0
    while s > 0 do
        s = shr(s, 1)
        c = c + 1
    end
    return c
end
local function tabsize(narr, nrec)
    return bor(narr, shl(hsize2hbits(nrec), 11))
end
function Ins.__index.tnewsize(narry, nhash)
    if narry then
        if narry < 3 then
            narry = 3
        elseif narry > 0x7ff then
            narry = 0x7ff
        end
    else
        narry = 0
    end
    return tabsize(narry, nhash or 0)
end

KObj = {}
KObj.__index = {}
function KObj.new(v)
    return setmetatable({ v }, KObj)
end
function KObj.__index:write(buf)
    local t, v = type(self[1]), self[1]
    if t == "string" then
        self:write_string(buf, v)
    elseif t == 'table' then
        if typeof(v) == Proto then
            self:write_proto(buf, v)
        else
            self:write_table(buf, v)
        end
    elseif t == 'cdata' then
        self:write_kcdata(buf, v)
    end
end
function KObj.__index:write_string(buf, v)
    buf:put_uleb128(KOBJ.STR + #v)
    buf:put_bytes(v)
end
function KObj.__index:write_kcdata(buf, v)
    if ffi.istype('double complex', v) then
        buf:put_uleb128(KOBJ.COMPLEX)
        local lo, hi = dword_get_u32(double_new, v[0])
        buf:put_uleb128(lo)
        buf:put_uleb128(hi)
        lo, hi = dword_get_u32(double_new, v[1])
        buf:put_uleb128(lo)
        buf:put_uleb128(hi)
    elseif ffi.istype('uint64_t', v) then
        buf:put_uleb128(KOBJ.U64)
        local lo, hi = dword_get_u32(uint64_new, v)
        buf:put_uleb128(lo)
        buf:put_uleb128(hi)
    elseif ffi.istype('int64_t', v) then
        buf:put_uleb128(KOBJ.I64)
        local lo, hi = dword_get_u32(int64_new, v)
        buf:put_uleb128(lo)
        buf:put_uleb128(hi)
    else
        assert(false, 'Unknown KCDATA : ' .. tostring(v))
    end
end

local function write_ktabk(buf, val, narrow)
    local tp = type(val)
    if tp == "string" then
        buf:put_uleb128(KTAB.STR + #val)
        buf:put_bytes(val)
    elseif tp == "number" then
        if narrow and num_is_int32(val) then
            buf:put(KTAB.INT)
            buf:put_uleb128(val)
            return
        end
        local lo, hi = dword_get_u32(double_new, val)
        buf:put(KTAB.NUM)
        buf:put_uleb128(lo)
        buf:put_uleb128(hi)
    elseif tp == "boolean" then
        buf:put(val and KTAB.TRUE or KTAB.FALSE)
    elseif tp == "nil" then
        buf:put(KTAB.NIL)
    else
        assert(false, "error with constant in ktabk")
    end
end

function KObj.__index:write_table(buf, v)
    buf:put_uleb128(KOBJ.TAB)
    buf:put_uleb128(v.narray)
    buf:put_uleb128(v.nhash)
    for i = 0, v.narray - 1 do
        write_ktabk(buf, v.array[i], true)
    end
    for i = 1, v.nhash do
        write_ktabk(buf, v.hash_keys[i], false)
        write_ktabk(buf, v.hash_values[i], true)
    end
end

KNum = {}
KNum.__index = {}
function KNum.new(v)
    return setmetatable({ v }, KNum)
end
function KNum.__index:write(buf)
    buf:put_number(self[1])
end

-- Determine if a backward jump up to "label_name" needs an UCLO instruction.
local function label_need_uclo(scope, label_name)
    local label_list = scope.goto_labels
    local need_uclo = false
    for i = #label_list, 1, -1 do
        local label = label_list[i]
        need_uclo = need_uclo or label.need_uclo
        if label.name == label_name then break end
    end
    return need_uclo
end

local function goto_label_resolve(self, label_name)
    local scope = self.scope
    while scope do
        local ls = scope.goto_labels
        for i = 1, #ls do
            if ls[i].name == label_name then
                return ls[i]
            end
        end
        scope = scope.outer
    end
end

local function goto_label_append(self, label_name)
    local scope = self.scope
    local label = { name = label_name, scope = scope, basereg = self.freereg, need_uclo = false, pc = #self.code }
    scope.goto_labels[#scope.goto_labels + 1] = label
    return label
end

local function goto_fixup_append(self, label_name, source_line, pc)
    local scope = self.scope
    local fixup = { name = label_name, basereg = self.freereg, need_uclo = false, pc = pc, source_line = source_line }
    scope.goto_fixups[#scope.goto_fixups + 1] = fixup
end

-- Follow the scope from the bottom (src_scope) to upper (dest_scope) to
-- tell if an UCLO instruction is needed to perform such a backward jump.
local function goto_uclo_backward(self, label, src_scope, dest_scope)
    local scope = src_scope
    local need_uclo = false
    while scope and scope ~= dest_scope do
        need_uclo = need_uclo or scope.need_uclo
        scope = scope.outer
    end
    if scope then
        need_uclo = need_uclo or label_need_uclo(scope, label.name)
    end
    return need_uclo
end

-- Bind dangling goto instructions ("fixups") to a label.
-- When a dangling goto is bound to a label it is removed from the
-- fixup list.
local function goto_label_bind(self, label_name)
    local fixup_list = self.scope.goto_fixups
    local i = 1
    while fixup_list[i] do
        local fixup = fixup_list[i]
        if fixup.name == label_name then
            if self.freereg > fixup.basereg then return fixup, fixup.basereg end
            self:fix_goto(fixup.pc, self.freereg, fixup.need_uclo, #self.code - fixup.pc)
            table.remove(fixup_list, i)
        else
            i = i + 1
        end
    end
end

Proto = {
    CHILD  = 0x01; -- Has child prototypes.
    VARARG = 0x02; -- Vararg function.
    FFI    = 0x04; -- Uses BC_KCDATA for FFI datatypes.
    NOJIT  = 0x08; -- JIT disabled for this function.
    ILOOP  = 0x10; -- Patched bytecode with ILOOP etc.
}
function Proto.new(flags, firstline, lastline, outer)
    local proto = setmetatable({
        flags  = flags or 0;
        outer  = outer;
        params = {};
        upvals = {};
        code   = {};
        kobj   = {};
        knum   = {};
        debug  = {};
        lninfo = {};
        labels = {};
        tohere = {};
        kcache = {};
        varinfo = {};
        freereg   = 0;
        firstline = firstline;
        lastline  = lastline;
        currline  = firstline;
        numlines  = lastline - firstline;
        framesize = 1;
        explret = false;
    }, Proto)

    proto:enter()
    return proto
end
Proto.__index = {}
function Proto.__index:nextreg(num)
    num = num or 1
    local reg = self.freereg
    self.freereg = self.freereg + num
    if self.freereg >= self.framesize then
        self.framesize = self.freereg
    end
    return reg
end
function Proto.__index:setreg(reg)
    self.freereg = reg
    if self.freereg >= self.framesize then
        self.framesize = self.freereg
    end
end
function Proto.__index:maxframe(reg)
    if reg > self.framesize then
        self.framesize = reg
    end
end

function Proto.__index:enter()
    local outer = self.scope
    self.scope = {
        actvars    = {};
        basereg    = self.freereg;
        goto_labels = {};
        goto_fixups = {};
        outer      = outer;
    }
    goto_label_append(self, nil) -- Add a label without name.
    return self.scope
end
function Proto.__index:is_root_scope()
    return (self.scope.outer == nil)
end
function Proto.__index:leave()
    for i=1, #self.scope.actvars do
        self.scope.actvars[i].endpc = #self.code
    end
    local freereg = self.scope.basereg
    self.scope = self.scope.outer
    self.freereg = freereg
end
function Proto.__index:child(firstline, lastline)
    self.flags = bor(self.flags, Proto.CHILD)
    local child = Proto.new(0, firstline, lastline, self)
    child.idx = #self.kobj
    self.kobj[child] = #self.kobj
    self.kobj[#self.kobj + 1] = child
    return child
end
function Proto.__index:parent()
    local parent = self.outer
    parent.flags = bor(parent.flags, band(self.flags, Proto.FFI))
    return parent
end
function Proto.__index:kpri(val)
    if val == nil then return VKNIL
    elseif val == true then return VKTRUE
    elseif val == false then return VKFALSE
    else error('Invalid primitive value: '..val) end
end
function Proto.__index:const(val)
    if type(val) == 'string' then
        if not self.kcache[val] then
            local item = KObj.new(val)
            item.idx = #self.kobj
            self.kcache[val] = item
            self.kobj[#self.kobj + 1] = item
        end
    elseif type(val) == 'number' then
        if not self.kcache[val] then
            local item = KNum.new(val)
            item.idx = #self.knum
            self.kcache[val] = item
            self.knum[#self.knum + 1] = item
        end
    elseif type(val) == 'cdata' then
        local item = KObj.new(val)
        item.idx = #self.kobj
        self.kobj[#self.kobj + 1] = item
        -- Set the FFI flag for the proto.
        self.flags = bor(self.flags, Proto.FFI)
        return item.idx
    else
        error("not a const: "..tostring(val))
    end
    return self.kcache[val].idx
end
function Proto.__index:new_table_template()
    local t = { array = {}, hash_keys = {}, hash_values = {} }
    local item = KObj.new(t)
    item.idx = #self.kobj
    self.kobj[#self.kobj + 1] = item
    return item.idx, t
end
function Proto.__index:line(ln)
    self.currline = ln
end
-- Set line number for the last generated instruction.
function Proto.__index:setpcline(ln)
    self.lninfo[#self.lninfo] = ln
end
function Proto.__index:emit(op, a, b, c)
    local ins = Ins.new(op, a, b, c)
    self.code[#self.code + 1] = ins
    self.lninfo[#self.lninfo + 1] = self.currline
    return ins
end
function Proto.__index:write(buf)
    local has_child
    if band(self.flags, Proto.CHILD) ~= 0 then
        has_child = true
        for i=1, #self.kobj do
            local o = self.kobj[i]
            if typeof(o) == Proto then
                o:write(buf)
            end
        end
    end

    local body = Buf.new()
    self:write_body(body)

    local offs = body.offs
    self:write_debug(body)

    local head = Buf.new()
    self:write_head(head, body.offs - offs)

    buf:put_uleb128(head.offs + body.offs) -- length of the proto

    local head_pack = ffi.string(head.data, head.offs)
    local body_pack = ffi.string(body.data, body.offs)

    buf:put_bytes(head_pack)
    buf:put_bytes(body_pack)
end
function Proto.__index:write_head(buf, size_debug)
    buf:put(self.flags)
    buf:put(#self.params)
    buf:put(self.framesize)
    buf:put(#self.upvals)
    buf:put_uleb128(#self.kobj)
    buf:put_uleb128(#self.knum)
    buf:put_uleb128(#self.code)
    buf:put_uleb128(size_debug or 0)
    buf:put_uleb128(self.firstline)
    buf:put_uleb128(self.numlines)
end
function Proto.__index:write_body(buf)
    for i=1, #self.code do
        self.code[i]:write(buf)
    end
    for i=1, #self.upvals do
        local uval = self.upvals[i]
        if uval.outer_idx then
            -- the upvalue refer to a local of the enclosing function
            local btag = uval.vinfo.mutable and 0x8000 or 0xc000
            local uv = bor(uval.outer_idx, btag)
            buf:put_uint16(uv)
        else
            -- the upvalue refer to an upvalue of the enclosing function
            local uv = uval.outer_uv
            buf:put_uint16(uv)
        end
    end
    for i=#self.kobj, 1, -1 do
        local o = self.kobj[i]
        if typeof(o) == Proto then
            buf:put_uleb128(KOBJ.CHILD)
        else
            self.kobj[i]:write(buf)
        end
    end
    for i=1, #self.knum do
        self.knum[i]:write(buf)
    end
end
function Proto.__index:write_debug(buf)
    local first = self.firstline
    if self.numlines < 256 then
        for i=1, #self.lninfo do
            local delta = self.lninfo[i] - first
            buf:put_uint8(delta)
        end
    elseif self.numlines < 65536 then
        for i=1, #self.lninfo do
            local delta = self.lninfo[i] - first
            buf:put_uint16(delta)
        end
    else
        for i=1, #self.lninfo do
            local delta = self.lninfo[i] - first
            buf:put_uint32(delta)
        end
    end
    for i=1, #self.upvals do
        local uval = self.upvals[i]
        buf:put_bytes(uval.vinfo.name.."\0")
    end
    local lastpc = 0
    for i=1, #self.varinfo do
        local var = self.varinfo[i]
        local startpc, endpc = (var.startpc or 0), (var.endpc or 0) + 1
        if type(var.name) == "number" then
            for n = var.name, var.name + 2 do
                buf:put(n)
                buf:put_uleb128(startpc - lastpc)
                buf:put_uleb128(endpc - startpc)
                lastpc = startpc
            end
        else
            buf:put_bytes(var.name.."\0")
            buf:put_uleb128(startpc - lastpc)
            buf:put_uleb128(endpc - startpc)
        end
        lastpc = startpc
    end
    buf:put(0)
end
function Proto.__index:newvar(name, dest)
    dest = dest or self:nextreg()
    local vinfo = {
        idx = dest,
        startpc = #self.code + 1,
        endpc = #self.code + 1,
        name = name,
        mutable = false,
    }
    -- scoped variable info
    self.scope.actvars[name] = vinfo
    self.scope.actvars[#self.scope.actvars + 1] = vinfo

    -- for the debug segment only
    self.varinfo[#self.varinfo + 1] = vinfo

    return vinfo
end
-- Add debug variables informations for implicit for variables.
-- The code should be 1 for simple "for" statements and 4 for
-- "for in" statements.
function Proto.__index:forivars(code)
    local vinfo = { name = code, startpc = #self.code + 1 }
    self.varinfo[#self.varinfo + 1] = vinfo
    return vinfo
end
local function scope_var_lookup(scope, name)
    while scope do
        local var = scope.actvars[name]
        if var then return var, scope end
        scope = scope.outer
    end
end
function Proto.__index:lookup(name)
    local var = scope_var_lookup(self.scope, name)
    if var then
        return var, false
    elseif self.outer then
        local v = self.outer:lookup(name)
        if v then return v, true end
    end
    -- Global variable.
    return nil, false
end
function Proto.__index:var_inverse_lookup(var)
    for i = 1, #self.scope.actvars do
        local xvar = self.scope.actvars[i]
        if xvar.idx == var then return xvar end
    end
end
function Proto.__index:param(...)
    local var = self:newvar(...)
    var.startpc = 0
    self.params[#self.params + 1] = var
    return var.idx
end
-- Find into the scope the label that immediately precedes where the given
-- variable is declared and return the label.
function Proto.__index:label_var_lookup(scope, var)
    local label_list = scope.goto_labels
    local label = label_list[1]
    for i = 2, #label_list do
        local next_label = label_list[i]
        if next_label.basereg > var.idx then break end
        label = next_label
    end
    return label
end
function Proto.__index:upval(name)
    if not self.upvals[name] then
        local proto, upval = self.outer, {}
        local var, scope
        while proto do
            var, scope = scope_var_lookup(proto.scope, name)
            if var then
                break
            end
            proto = proto.outer
        end

        upval = { vinfo = var; proto = proto; }

        -- for each upval we set either outer_idx or outer_uv
        if proto == self.outer then
            -- The variable is in the enclosing function's scope.
            -- We store just its register index.
            upval.outer_idx = var.idx
            local label = self:label_var_lookup(scope, var)
            label.need_uclo = true
            scope.need_uclo = true
        else
            -- The variable is in the outer scope of the enclosing
            -- function. We register this variable as an upvalue for
            -- the enclosing function. Then we store the upvale index.
            upval.outer_uv = self.outer:upval(name)
        end

        self.upvals[name] = upval
        upval.idx = #self.upvals
        self.upvals[#self.upvals + 1] = upval
    end
    return self.upvals[name].idx
end
function Proto.__index:here(name)
    if self.tohere[name] then
        -- forward jump
        local back = self.tohere[name]
        for i=1, #back do
            local offs = back[i]
            self.code[offs][3] = #self.code - offs
        end
        self.tohere[name] = nil
    else
        -- backward jump
        self.labels[name] = #self.code - 1
    end
    return name
end
function Proto.__index:enable_jump(name)
    if type(name) == 'number' then
        error("bad label")
    end
    local here = self.tohere[name]
    if not here then
        here = {}
        self.tohere[name] = here
    end
    here[#here + 1] = #self.code + 1
end
local function locate_jump(self, name)
    if self.labels[name] then
        -- backward jump
        return self.labels[name] - #self.code
    else
        -- forward jump
        self:enable_jump(name)
        return NO_JMP
    end
end
function Proto.__index:scope_jump(name, basereg, need_uclo)
    local jump = locate_jump(self, name)
    return self:emit(need_uclo and BC.UCLO or BC.JMP, basereg, jump)
end
function Proto.__index:jump(name, basereg)
    local jump = locate_jump(self, name)
    return self:emit(BC.JMP, basereg, jump)
end
-- When closing a scope this function is used to transfer all the dangling
-- gotos (fixups) in the enclosing scope.
-- Before being copied the basereg and need_uclo information is updated.
-- The original list is not modified since it will be dropped with the scope
-- itself.
function Proto.__index:fscope_end()
    local scope = self.scope
    local fixup_list = scope.goto_fixups
    local outer_list = scope.outer.goto_fixups
    for i = 1, #fixup_list do
        local fixup = fixup_list[i]
        fixup.need_uclo = fixup.need_uclo or scope.need_uclo
        fixup.basereg = scope.basereg
        outer_list[#outer_list + 1] = fixup
    end
end
function Proto.__index:fix_goto(pc, basereg, need_uclo, jump)
    local ins = self.code[pc]
    local op = need_uclo and BC.UCLO or BC.JMP
    ins:rewrite(op, basereg, jump)
end
function Proto.__index:goto_jump(label_name, source_line)
    local label = goto_label_resolve(self, label_name)
    if label then -- backward jump
        local dest_scope, basereg = label.scope, label.basereg
        local need_uclo = goto_uclo_backward(self, label, self.scope, dest_scope)
        local op = need_uclo and BC.UCLO or BC.JMP
        self:emit(op, basereg, label.pc - #self.code - 1)
    else -- forward jump
        self:emit(BC.JMP, 0, NO_JMP)
        goto_fixup_append(self, label_name, source_line, #self.code)
    end
end
function Proto.__index:goto_label(label_name)
    if goto_label_resolve(self, label_name) then
        return false, string.format("duplicate label '%s'", label_name)
    end
    local label = goto_label_append(self, label_name)
    local goto_err, var_err = goto_label_bind(self, label_name)
    if goto_err then
        local var = self:var_inverse_lookup(var_err)
        return false, string.format("<goto %s> jumps into the scope of local '%s'", label_name, var.name)
    else
        return true, label
    end
end
function Proto.__index:loop_register(exit, exit_reg, cont)
    self.scope.loop_exit = exit
    self.scope.loop_basereg = exit_reg
    self.scope.loop_cont = cont
end
function Proto.__index:current_loop(cont)
    local scope = self.scope
    local need_uclo = false
    while scope do
        need_uclo = need_uclo or scope.need_uclo
        if cont then
            if scope.loop_cont then break end
        else
            if scope.loop_exit then break end
        end
        scope = scope.outer
    end
    assert(scope, "no loop to " .. (cont and "continue" or "break"))
    return scope.loop_basereg, cont and scope.loop_cont or scope.loop_exit,
        need_uclo
end
function Proto.__index:global_uclo()
    local scope = self.scope
    while scope do
        if scope.need_uclo then return true end
        scope = scope.outer
    end
end
function Proto.__index:loop(name)
    if self.labels[name] then
        -- backward jump
        local offs = self.labels[name]
        return self:emit(BC.LOOP, self.freereg, offs - #self.code)
    else
        -- forward jump
        self:enable_jump(name)
        return self:emit(BC.LOOP, self.freereg, NO_JMP)
    end
end
function Proto.__index:op_jump(jump, base)
    return self:emit(BC.JMP, base, jump)
end
function Proto.__index:op_loop(delta)
    return self:emit(BC.LOOP, self.freereg, delta)
end

-- branch if condition
function Proto.__index:op_test(cond, a, here, freereg)
    local inst = self:emit(cond and BC.IST or BC.ISF, 0, a)
    if here then here = self:jump(here, freereg) end
    return inst, here
end
function Proto.__index:op_testmov(cond, dest, var, here, freereg)
    local inst = self:emit(cond and BC.ISTC or BC.ISFC, dest, var)
    if here then here = self:jump(here, freereg) end
    return inst, here
end
function Proto.__index:op_comp(cond, a, btag, b, here, freereg, swap)
    local suffix = (cond == 'EQ' or cond == 'NE') and btag or ''
    local ins = BC['IS'..cond..suffix]
    assert(suffix == '' or not swap, "Cannot swap comparison for VN equality test")
    if swap then self:emit(ins, b, a) else self:emit(ins, a, b) end
    self:jump(here, freereg)
end

function Proto.__index:op_infix(opname, dest, v1tag, var1, v2tag, var2)
    local ins = BC[opname .. v1tag .. v2tag]
    assert(ins, "Invalid operation: " .. opname)
    if v1tag == 'N' then
        var1, var2 = var2, var1
    end
    return self:emit(ins, dest, var1, var2)
end
function Proto.__index:op_add(dest, var1, var2)
    return self:emit(BC.ADDVV, dest, var1, var2)
end
function Proto.__index:op_sub(dest, var1, var2)
    return self:emit(BC.SUBVV, dest, var1, var2)
end
function Proto.__index:op_mul(dest, var1, var2)
    return self:emit(BC.MULVV, dest, var1, var2)
end
function Proto.__index:op_div(dest, var1, var2)
    return self:emit(BC.DIVVV, dest, var1, var2)
end
function Proto.__index:op_mod(dest, var1, var2)
    return self:emit(BC.MODVV, dest, var1, var2)
end
function Proto.__index:op_pow(dest, var1, var2)
    return self:emit(BC.POW, dest, var1, var2)
end
function Proto.__index:op_gget(dest, name)
    return self:emit(BC.GGET, dest, self:const(name))
end
function Proto.__index:op_gset(from, name)
    return self:emit(BC.GSET, from, self:const(name))
end

function Proto.__index:op_not(dest, var1)
    return self:emit(BC.NOT, dest, var1)
end
function Proto.__index:op_unm(dest, var1)
    return self:emit(BC.UNM, dest, var1)
end
function Proto.__index:op_len(dest, var1)
    return self:emit(BC.LEN, dest, var1)
end
function Proto.__index:op_nils(dest, want)
    if want == 1 then
        return self:emit(BC.KPRI, dest, VKNIL)
    elseif want > 1 then
        return self:emit(BC.KNIL, dest, dest + want - 1)
    end
end

function Proto.__index:op_move(dest, from)
    return self:emit(BC.MOV, dest, from)
end
function Proto.__index:op_load(dest, val)
    local tv = type(val)
    if tv == 'nil' then
        return self:emit(BC.KPRI, dest, VKNIL)
    elseif tv == 'boolean' then
        return self:emit(BC.KPRI, dest, val and VKTRUE or VKFALSE)
    elseif tv == 'string' then
        return self:emit(BC.KSTR, dest, self:const(val))
    elseif tv == 'number' then
        if math.floor(val) == val and val < 0x8000 and val >= -0x8000 then
            return self:emit(BC.KSHORT, dest, val)
        else
            return self:emit(BC.KNUM, dest, self:const(val))
        end
    elseif tv == 'cdata' then
        self:emit(BC.KCDATA, dest, self:const(val))
    else
        error("cannot load as constant: "..tostring(val))
    end
end
function Proto.__index:op_tdup(dest, index)
    return self:emit(BC.TDUP, dest, index)
end
function Proto.__index:op_tnew(dest, narr, nrec)
    return self:emit(BC.TNEW, dest, tabsize(narr or 0, nrec or 0))
end
function Proto.__index:op_tget(dest, tab, ktag, key)
    local ins_name = 'TGET' .. ktag
    self:emit(BC[ins_name], dest, tab, key)
end
function Proto.__index:op_tset(tab, ktag, key, val)
    local ins_name = 'TSET' .. ktag
    self:emit(BC[ins_name], val, tab, key)
end
function Proto.__index:op_tsetm(base, vnum)
    local dptr = double_new(0)
    local iptr = ffi.cast('uint32_t*', dptr)
    iptr[0] = vnum
    iptr[1] = 0x43300000 -- Biased integer to avoid denormals.
    local vidx = self:const(dptr[0])
    return self:emit(BC.TSETM, base + 1, vidx)
end
function Proto.__index:op_fnew(dest, pidx)
    return self:emit(BC.FNEW, dest, pidx)
end
function Proto.__index:op_uclo(base, jump)
    return self:emit(BC.UCLO, base, jump)
end
function Proto.__index:op_uset(uv, vtag, val)
    local ins = BC['USET' .. vtag]
    self:emit(ins, uv, val)
end
function Proto.__index:op_uget(dest, uv)
    return self:emit(BC.UGET, dest, uv)
end
-- Generate the UCLO + JMP instruction at the end of a block.
-- The UCLO is generated only if there are open upvalues. The "exit"
-- parameter is optional. If omitted there will be no JMP instruction.
function Proto.__index:close_block(reg, exit)
    if self.scope.need_uclo then
        if exit then
            self:enable_jump(exit)
            self:emit(BC.UCLO, reg, NO_JMP)
        else
            self:emit(BC.UCLO, reg, 0)
        end
    else
        if exit then
            self:enable_jump(exit)
            return self:emit(BC.JMP, reg, NO_JMP)
        end
    end
end
function Proto.__index:close_uvals()
    if self:global_uclo() then
        self:emit(BC.UCLO, 0, 0)
    end
end
function Proto.__index:close_proto()
    if not self.explret then
        self:close_uvals()
        self:op_ret0()
    end
    local fixups = self.scope.goto_fixups
    if #fixups > 0 then
        local label = fixups[1]
        local msg = string.format("undefined label '%s'", label.name)
        return msg, label.source_line
    end
    self:leave()
end
function Proto.__index:op_ret(base, rnum)
    return self:emit(BC.RET, base, rnum + 1)
end
function Proto.__index:op_ret0()
    return self:emit(BC.RET0, 0, 1)
end
function Proto.__index:op_ret1(base)
    return self:emit(BC.RET1, base, 2)
end
function Proto.__index:op_retm(base, rnum)
    return self:emit(BC.RETM, base, rnum)
end
function Proto.__index:op_varg(base, want)
    return self:emit(BC.VARG, base, want + 1, #self.params)
end
function Proto.__index:op_call(base, want, narg)
    return self:emit(BC.CALL, base, want + 1, narg + 1)
end
function Proto.__index:op_callt(base, narg)
    return self:emit(BC.CALLT, base, narg + 1)
end
function Proto.__index:op_callm(base, want, narg)
    return self:emit(BC.CALLM, base, want + 1, narg)
end
function Proto.__index:op_callmt(base, narg)
    return self:emit(BC.CALLMT, base, narg)
end
function Proto.__index:op_fori(base, stop, step)
    local loop = self:emit(BC.FORI, base, NO_JMP)
    self:here(loop)
    return loop
end
function Proto.__index:op_forl(base, loop)
    local offs = self.labels[loop]
    loop[3] = #self.code - offs
    return self:emit(BC.FORL, base, offs - #self.code)
end
function Proto.__index:op_iterc(base, want)
    return self:emit(BC.ITERC, base, want + 1, 3)
end
function Proto.__index:op_iterl(base, loop)
    local offs = self.labels[loop]
    return self:emit(BC.ITERL, base, offs - #self.code)
end
function Proto.__index:op_cat(base, rbot, rtop)
    return self:emit(BC.CAT, base, rbot, rtop)
end

Dump = {
    HEAD_1 = 0x1b;
    HEAD_2 = 0x4c;
    HEAD_3 = 0x4a;
    VERS   = jit_v21 and 0x02 or 0x01;
    BE     = 0x01;
    STRIP  = 0x02;
    FFI    = 0x04;
    DEBUG  = false;
}
Dump.__index = {}
function Dump.new(main, name)
    local self =  setmetatable({
        main  = main;
        name  = name;
        flags = band(main.flags, Dump.FFI);
    }, Dump)
    return self
end
function Dump.__index:write_header(buf)
    buf:put(Dump.HEAD_1)
    buf:put(Dump.HEAD_2)
    buf:put(Dump.HEAD_3)
    buf:put(Dump.VERS)
    buf:put(self.flags)
    local name = self.name and "@" .. self.name or "(binary)"
    if band(self.flags, Dump.STRIP) == 0 then
        buf:put_uleb128(#name)
        buf:put_bytes(name)
    end
end
function Dump.__index:write_footer(buf)
    buf:put(0x00)
end
function Dump.__index:pack()
    local buf = Buf.new()
    self:write_header(buf)
    self.main:write(buf)
    self:write_footer(buf)
    return buf:pack()
end

return {
    Buf   = Buf;
    Ins   = Ins;
    KNum  = KNum;
    KObj  = KObj;
    Proto = Proto;
    Dump  = Dump;
    BC    = BC;
}

