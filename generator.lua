--
-- LuaJIT Language Toolkit.
--
-- Copyright (C) 2013-2014 Francesco Abbate. All rights reserved.
--
-- Based on the original work of Richard Hundt,
-- https://github.com/richardhundt/nyanga.
--
-- See Copyright Notice in LICENSE
--

local bc   = require('bytecode')
local util = require('util')

local BC = bc.BC

-- comparison operators with corresponding instruction.
-- the boolean value indicate if the operands should be swapped.
local cmpop = {
   ['<' ] = { 'LT', false },
   ['>' ] = { 'LT', true  },
   ['<='] = { 'LE', false },
   ['>='] = { 'LE', true  },
   ['=='] = { 'EQ', false },
   ['~='] = { 'NE', false },
}

-- the same of above but for the inverse tests
local cmpopinv = {
   ['<' ] = { 'GE', false },
   ['>' ] = { 'GE', true  },
   ['<='] = { 'GT', false },
   ['>='] = { 'GT', true  },
   ['=='] = { 'NE', false },
   ['~='] = { 'EQ', false },
}

local MULTIRES = -1

-- this should be considered like binary values to perform
-- bitfield operations
local EXPR_RESULT_TRUE, EXPR_RESULT_FALSE = 1, 2
local EXPR_RESULT_BOTH = 3

-- Infix arithmetic instructions
local EXPR_EMIT_VN   = { value = true, number = true }

-- USETx, ISEQx and ISNEx instructions
local EXPR_EMIT_VSNP = { value = true, string = true, number = true, primitive = true }

-- TGETx/TSETx instructions
local EXPR_EMIT_VSB  = { value = true, string = true, byte = true }

local StatementRule = { }
local ExpressionRule = { }
local MultiExprRule = { }
local LHSExpressionRule = { }
local TestRule = { }
local ConstRule = { }

local function is_literal(node)
   return node.kind == 'Literal'
end

local function is_identifier(node)
   return node.kind == 'Identifier'
end

local function is_local_var(ctx, node)
   if node.kind == 'Identifier' then
      local info, uval = ctx:lookup(node.name)
      if info and not uval then
         return info.idx
      end
   end
end

local function mov_toreg(ctx, dest, src)
   if dest ~= src then
      ctx:op_move(dest, src)
   end
end

local function is_byte_number(v)
   return type(v) == 'number' and v % 1 == 0 and v >= 0 and v < 256
end

-- ExpressionRule's entries take a node and a destination register (dest)
-- used to store the result. At the end of the call no new registers are
-- marked as used.
-- ExpressionRule functions return nothing or a boolean value to indicate if
-- a the expression terminate with a tail call instruction.

function ExpressionRule:Literal(node, dest)
   self.ctx:op_load(dest, node.value)
end

function ExpressionRule:Identifier(node, dest)
   local name = node.name
   local var, uval = self.ctx:lookup(name)
   if var then
      if uval then
         -- Ensure variable is marked as upvalue in proto in take
         -- the upvalue index.
         local uv = self.ctx:upval(name)
         self.ctx:op_uget(dest, uv)
      else
         mov_toreg(self.ctx, dest, var.idx)
      end
   else
      self.ctx:op_gget(dest, name)
   end
end

function ExpressionRule:Vararg(node, dest)
   self.ctx:op_varg(dest, 1)
end

-- MultiExprRule's entries take a node and a number of wanted results (want)
-- and an optional boolean argument "tail" that indicate to emit tail call
-- if possible.
-- The argument "want" can also be MULTIRES to indicate that the caller want
-- as many results as the instructions returns.
-- The code will store on the stack (starting from freereg) the number of
-- wanted results.
-- Return a first boolean value to indicate if many results are generated.
-- A second boolean value indicate if a tail call was actually done.

function MultiExprRule:Vararg(node, want)
   self.ctx:op_varg(self.ctx.freereg, want)
   return true, false -- Multiple results, no tail call.
end

local function expr_isk(node)
   if node.kind == "Literal" then
      local t = type(node.value)
      return (t == "string" or t == "number" or t == "boolean" or t == "nil")
   else
      return false
   end
end

local function emit_tdup(self, dest, ins)
   local kidx, t = self.ctx:new_table_template()
   ins:rewrite(BC.TDUP, dest, kidx)
   return t
end

function ExpressionRule:Table(node, dest)
   local free = self.ctx.freereg
   local ins = self.ctx:op_tnew(free, 0)
   self.ctx:nextreg()
   local t
   local vtop = self.ctx.freereg
   local narray, nhash = 0, 0
   local zeroarr = 0
   for k = 1, #node.array_entries do
      local expr = node.array_entries[k]
      if expr_isk(expr) then
         if not t then t = emit_tdup(self, free, ins) end
         t.array[k] = expr.value
         narray = k + 1
      else
         local ktag, kval
         if k < 256 then
            ktag, kval = 'B', k
         else
            ktag, kval = 'V', self.ctx:nextreg()
            self.ctx:op_load(kval, k)
         end
         local v = self:expr_toanyreg(expr)
         self.ctx:op_tset(free, ktag, kval, v)
         self.ctx.freereg = vtop
      end
   end

   for i = 1, #node.hash_keys do
      local key, value = node.hash_keys[i], node.hash_values[i]
      if expr_isk(key) and key.value ~= nil and expr_isk(value) then
         local kval, vval = key.value, value.value
         if type(kval) == "number" and kval == 0 then
            if not t then t = emit_tdup(self, free, ins) end
            t.array[kval] = vval
            narray = math.max(narray, 1)
            zeroarr = 1
         else
            nhash = nhash + 1
            if not t then t = emit_tdup(self, free, ins) end
            t.hash_keys[nhash] = kval
            t.hash_values[nhash] = vval
         end
      else
         local ktag, kval = self:expr_toanyreg_tagged(key, EXPR_EMIT_VSB)
         local v = self:expr_toanyreg(value)
         self.ctx:op_tset(free, ktag, kval, v)
         self.ctx.freereg = vtop
      end
   end

   if t then
      t.narray, t.nhash = narray, nhash
   else
      local na = #node.array_entries + zeroarr
      local nh = #node.hash_keys - zeroarr
      local sz = ins.tnewsize(na > 0 and na or nil, nh)
      ins:rewrite(BC.TNEW, free, sz)
   end

   mov_toreg(self.ctx, dest, free)

   self.ctx.freereg = free
end

-- Operations that admit instructions in the form ADDVV, ADDVN, ADDNV
local dirop = {
   ['+'] = 'ADD',
   ['*'] = 'MUL',
   ['-'] = 'SUB',
   ['/'] = 'DIV',
   ['%'] = 'MOD',
}

local function dirop_compute(o, a, b)
   if     o == '+' then return a + b
   elseif o == '-' then return a - b
   elseif o == '*' then return a * b
   elseif o == '/' and b ~= 0 then return a / b
   elseif o == '%' then return a % b
   elseif o == '^' then return a ^ b
   end
end

function ExpressionRule:ConcatenateExpression(node, dest)
   local free = self.ctx.freereg
   for i = 0, #node.terms - 1 do
      self:expr_toreg(node.terms[i + 1], free + i)
      self.ctx:setreg(free + i + 1)
   end
   self.ctx.freereg = free
   self.ctx:op_cat(dest, free, free + #node.terms - 1)
end

function ExpressionRule:BinaryExpression(node, dest)
   local o = node.operator
   if cmpop[o] then
      local l = util.genid()
      self:test_emit(node, l, false, EXPR_RESULT_BOTH, dest)
      self.ctx:here(l)
   elseif dirop[o] then
      local free = self.ctx.freereg
      local atag, a = self:expr_toanyreg_tagged(node.left, EXPR_EMIT_VN)
      local btag, b = self:expr_toanyreg_tagged(node.right, EXPR_EMIT_VN)
      self.ctx.freereg = free
      assert(not (atag == 'N' and btag == 'N'), "operands are both constants")
      self.ctx:op_infix(dirop[o], dest, atag, a, btag, b)
   else
      local free = self.ctx.freereg
      local a = self:expr_toanyreg(node.left)
      local b = self:expr_toanyreg(node.right)
      self.ctx.freereg = free
      if o == '^' then
         self.ctx:op_pow(dest, a, b)
      else
         error("bad binary operator: "..o, 2)
      end
   end
end

function ExpressionRule:UnaryExpression(node, dest)
   local free = self.ctx.freereg
   local a = self:expr_toanyreg(node.argument)
   self.ctx.freereg = free
   local o = node.operator
   if o == '-' then
      self.ctx:op_unm(dest, a)
   elseif o == '#' then
      self.ctx:op_len(dest, a)
   elseif o == 'not' then
      self.ctx:op_not(dest, a)
   else
      error("bad unary operator: "..o, 2)
   end
end

function ExpressionRule:LogicalExpression(node, dest)
   local negate = (node.operator == 'or')
   local lstore = (node.operator == 'or' and EXPR_RESULT_TRUE or EXPR_RESULT_FALSE)
   local l = util.genid()
   self:test_emit(node.left, l, negate, lstore, dest)
   self:expr_toreg(node.right, dest)
   self.ctx:here(l)
end

function ExpressionRule:MemberExpression(node, dest)
   local free = self.ctx.freereg
   local lhs = self:lhs_expr_emit(node)
   self.ctx.freereg = free
   self.ctx:op_tget(dest, lhs.target, lhs.key_type, lhs.key)
end

function StatementRule:FunctionDeclaration(node)
   local name = node.id.name
   local dest
   if node.locald then
      dest = self.ctx:newvar(name).idx
   else
      dest = self.ctx.freereg
   end

   ExpressionRule.FunctionExpression(self, node, dest)
end

function ExpressionRule:FunctionExpression(node, dest)
   local free = self.ctx.freereg
   local func = self.ctx:child()
   self.ctx = func
   for i=1, #node.params do
      if node.params[i].kind == 'Vararg' then
         self.ctx.flags = bit.bor(self.ctx.flags, bc.Proto.VARARG)
      else
         self.ctx:param(node.params[i].name)
      end
   end
   self:emit(node.body)
   self:close_proto()
   self.ctx:set_line(node.firstline, node.lastline)

   self.ctx = self.ctx.outer
   self.ctx.freereg = free
   self.ctx:op_fnew(dest, func.idx)
end

local function emit_call_expression(self, node, want, use_tail, use_self)
   local base = self.ctx.freereg
   local free = base

   if use_self then
      local obj = self:expr_toanyreg(node.receiver)
      self.ctx:setreg(base + 1)
      self.ctx:op_move(base + 1, obj)
      local method = self.ctx:const(node.method.name)
      self.ctx:op_tget(base, obj, 'S', method)
      self.ctx:nextreg()
   else
      self:expr_toreg(node.callee, base)
      self.ctx:nextreg()
   end

   local narg = #node.arguments
   for i=1, narg - 1 do
      self:expr_toreg(node.arguments[i], self.ctx.freereg)
      self.ctx:nextreg()
   end
   local mres = false
   if narg > 0 then
      local lastarg = node.arguments[narg]
      mres = self:expr_tomultireg(lastarg, MULTIRES)
   end

   if use_self then narg = narg + 1 end
   self.ctx.freereg = free
   if mres then
      if use_tail then
         self.ctx:close_uvals()
         self.ctx:op_callmt(base, narg - 1)
      else
         self.ctx:op_callm(base, want, narg - 1)
      end
   else
      if use_tail then
         self.ctx:close_uvals()
         self.ctx:op_callt(base, narg)
      else
         self.ctx:op_call(base, want, narg)
      end
   end

   return want == MULTIRES, use_tail
end

function MultiExprRule:CallExpression(node, want, tail)
   return emit_call_expression(self, node, want, tail, false)
end

function ExpressionRule:SendExpression(node, want, tail)
   return emit_call_expression(self, node, want, tail, true)
end

function LHSExpressionRule:Identifier(node)
   local info, uval = self.ctx:lookup(node.name)
   if uval then
      -- Ensure variable is marked as upvalue in proto and take
      -- upvalue index.
      local uv = self.ctx:upval(node.name)
      return {tag = 'upval', uv = uv}
   elseif info then
      return {tag = 'local', target = info.idx}
   else
      return {tag = 'global', name = node.name}
   end
end

function LHSExpressionRule:MemberExpression(node)
   local target = self:expr_toanyreg(node.object)
   local key_type, key
   if node.computed then
      key_type, key = self:expr_toanyreg_tagged(node.property, EXPR_EMIT_VSB)
   else
      key_type, key = 'S', self.ctx:const(node.property.name)
   end
   return { tag = 'member', target = target, key = key, key_type = key_type }
end

function TestRule:Identifier(node, jmp, negate, store, dest)
   local var = is_local_var(self.ctx, node)
   if var then
      local jreg = store ~= 0 and dest + 1 or self.ctx.freereg
      if store ~= 0 and dest ~= var then
         self.ctx:op_testmov(negate, dest, var, jmp, jreg)
      else
         self.ctx:op_test(negate, var, jmp, jreg)
      end
   else
      self:expr_test(node, jmp, negate, store, dest)
   end
end

function TestRule:Literal(node, jmp, negate, store, dest)
   local free = self.ctx.freereg
   local value = node.value
   if bit.band(store, value and EXPR_RESULT_TRUE or EXPR_RESULT_FALSE) ~= 0 then
      self:expr_toreg(node, dest)
      self.ctx:nextreg()
   end
   if (negate and value) or (not negate and not value) then
      local jreg = store ~= 0 and dest + 1 or free
      self.ctx:jump(jmp, jreg)
   end
   self.ctx.freereg = free
end

local function lookup_test(negate, op)
   local lookup = negate and cmpop or cmpopinv
   return unpack(lookup[op])
end

-- Return true IFF the variable "store" has the EXPR_RESULT_FALSE bit
-- set. If "negate" is true check the EXPR_RESULT_TRUE bit instead.
local function has_branch(store, negate)
   return bit.band(store, negate and EXPR_RESULT_TRUE or EXPR_RESULT_FALSE) ~= 0
end

function TestRule:BinaryExpression(node, jmp, negate, store, dest)
   local o = node.operator
   if cmpop[o] then
      local free = self.ctx.freereg
      local atag, a, btag, b
      if o == '==' or o == '~=' then
         atag, a = self:expr_toanyreg_tagged(node.left, EXPR_EMIT_VSNP)
         if atag == 'V' then
            btag, b = self:expr_toanyreg_tagged(node.right, EXPR_EMIT_VSNP)
         else
            btag, b = atag, a
            atag, a = 'V', self:expr_toanyreg(node.right)
         end
      else
         a = self:expr_toanyreg(node.left)
         b = self:expr_toanyreg(node.right)
      end
      self.ctx.freereg = free
      local use_imbranch = (dest and has_branch(store, negate))
      if use_imbranch then
         local jreg = store ~= 0 and dest + 1 or free
         local test, swap = lookup_test(not negate, o)
         local altlabel = util.genid()
         self.ctx:op_comp(test, a, btag, b, altlabel, free, swap)
         self.ctx:op_load(dest, negate)
         self.ctx:jump(jmp, jreg)
         self.ctx:here(altlabel)
         self.ctx.freereg = free
      else
         local test, swap = lookup_test(negate, o)
         self.ctx:op_comp(test, a, btag, b, jmp, free, swap)
      end
      if has_branch(store, not negate) then
         self.ctx:op_load(dest, not negate)
      end
   else
      -- LuaJIT compatibility rule: set store to a non zero
      -- value so that the register is counted on test jump
      store, dest = EXPR_RESULT_BOTH, dest or self.ctx.freereg
      self:expr_test(node, jmp, negate, store, dest)
   end
end

function TestRule:UnaryExpression(node, jmp, negate, store, dest)
   if node.operator == 'not' then
      self:test_emit(node.argument, jmp, not negate, store, dest)
      if dest and store ~= 0 then
         self:op_not(dest, dest)
      end
   else
      self:expr_test(node, jmp, negate, store, dest or self.ctx.freereg)
   end
end

function TestRule:LogicalExpression(node, jmp, negate, store, dest)
   local o = node.operator
   local lbit = o == 'and' and EXPR_RESULT_FALSE or EXPR_RESULT_TRUE
   local lstore = bit.band(store, lbit)
   local imbranch = (o == 'and' and negate) or (o == 'or' and not negate)
   if imbranch then
      local templ = util.genid()
      self:test_emit(node.left, templ, not negate, lstore, dest)
      self:test_emit(node.right, jmp, negate, store, dest)
      self.ctx:here(templ)
   else
      self:test_emit(node.left, jmp, negate, lstore, dest)
      self:test_emit(node.right, jmp, negate, store, dest)
   end
end

function ConstRule:Literal(node)
   local v = node.value
   if type(v) == 'number' then return v end
end

function ConstRule:BinaryExpression(node)
   local o = node.operator
   local a = self:const_eval_try(node.left)
   if a then
      local b = self:const_eval_try(node.right)
      if b then
         return dirop_compute(o, a, b)
      end
   end
end

function ConstRule:UnaryExpression(node)
   local o = node.operator
   if o == '-' then
      local v = self:const_eval_try(node.argument)
      if v then return -v end
   end
end

function StatementRule:CallExpression(node)
   self:expr_tomultireg(node, 0, false)
end

function StatementRule:SendExpression(node)
   self:expr_tomultireg(node, 0, false)
end

function StatementRule:LabelStatement(node)
   return self.ctx:here(node.label)
end

function StatementRule:GotoStatement(node)
   return self.ctx:jump(node.label)
end

function StatementRule:BlockStatement(node, if_exit)
   local body = node.body
   for i=1, #body - 1 do
      self:emit(body[i])
   end
   self:emit(body[#body], if_exit)
end

function StatementRule:DoStatement(node)
   self:block_enter()
   self:emit(node.body)
   self:block_leave()
end

function StatementRule:IfStatement(node, root_exit)
   local free = self.ctx.freereg
   local ncons = #node.tests
   -- Count the number of branches, including the "else" branch.
   local count = node.alternate and ncons + 1 or ncons
   local local_exit = count > 1 and util.genid()
   -- Set the exit point to the extern exit if given or set to local
   -- exit (potentially false).
   local exit = root_exit or local_exit

   for i = 1, ncons do
      local test, block = node.tests[i], node.cons[i]
      local next_test = util.genid()
      -- Set the exit point to jump on at the end of for this block.
      -- If this is the last branch (count == 1) set to false.
      local bexit = count > 1 and exit

      self:test_emit(test, next_test)

      self:block_enter()
      self:emit(block, bexit)
      self:block_leave(bexit)

      self.ctx:here(next_test)
      count = count - 1
   end

   if node.alternate then
      self:block_enter()
      self:emit(node.alternate)
      self:block_leave()
   end
   if exit and exit == local_exit then
      self.ctx:here(exit)
   end
   self.ctx.freereg = free
end
function StatementRule:ExpressionStatement(node)
   return self:emit(node.expression)
end
function StatementRule:LocalDeclaration(node)
   local nvars = #node.names
   local nexps = #node.expressions
   local base = self.ctx.freereg
   local slots = nvars
   for i = 1, nexps - 1 do
      if slots == 0 then break end
      self:expr_toreg(node.expressions[i], self.ctx.freereg)
      self.ctx:nextreg()
      slots = slots - 1
   end

   if slots > 0 then
      if nexps > 0 then
         self:expr_tomultireg(node.expressions[nexps], slots)
      else
         self.ctx:op_nils(base, slots)
      end
      self.ctx:nextreg(slots)
   end

   for i=1, nvars do
      local lhs = node.names[i]
      self.ctx:newvar(lhs.name, base + (i - 1))
   end
end

function StatementRule:AssignmentExpression(node)
   local free = self.ctx.freereg
   local nvars = #node.left
   local nexps = #node.right

   local lhs = { }
   for i = 1, nvars do
      lhs[i] = self:lhs_expr_emit(node.left[i])
   end

   local slots = nvars
   local exprs = { }
   for i=1, nexps - 1 do
      if slots == 0 then break end
      -- LuaJIT compatibility:
      -- Use a temporary register even the LHS is not an immediate local
      -- variable.
      local use_reg = true
      --[[
      local use_reg = is_local_var(self.ctx, node.left[i])
      ]]
      exprs[i] = use_reg and self.ctx.freereg
      self:expr_toreg(node.right[i], exprs[i])
      if use_reg then self.ctx:nextreg() end
      slots = slots - 1
   end

   local i = nexps
   if slots == 1 then
      if lhs[i].tag == 'upval' then
         local tag, expr = self:expr_toanyreg_tagged(node.right[i], EXPR_EMIT_VSNP)
         self.ctx:op_uset(lhs[i].uv, tag, expr)
         nvars = nvars - 1
      elseif lhs[i].tag == 'local' then
         self:expr_toreg(node.right[i], lhs[i].target)
         nvars = nvars - 1
      else
         exprs[i] = self:expr_toanyreg(node.right[i])
      end
   else
      local exp_base = self.ctx.freereg
      self:expr_tomultireg(node.right[i], slots)
      for k = slots - 1, 0, -1 do
         self:assign(lhs[i + k], exp_base + k)
      end
      nvars = nvars - slots
   end

   for i = nvars, 1, -1 do
      self:assign(lhs[i], exprs[i])
   end

   self.ctx.freereg = free
end
function StatementRule:WhileStatement(node)
   local free = self.ctx.freereg
   local loop, exit = util.genid(), util.genid()
   local save_exit, save_exit_reg = self:loop_enter(exit, free)
   self.ctx:here(loop)
   self:test_emit(node.test, exit)
   self.ctx:loop(exit)
   self:emit(node.body)
   self.ctx:jump(loop, free)
   self.ctx:here(exit)
   self:loop_leave(save_exit, save_exit_reg)
   self.ctx.freereg = free
end
function StatementRule:RepeatStatement(node)
   local free = self.ctx.freereg
   local loop, exit = util.genid(), util.genid()
   local save_exit, save_exit_reg = self:loop_enter(exit, free)
   self.ctx:here(loop)
   self.ctx:loop(exit)
   self:emit(node.body)
   self:test_emit(node.test, loop)
   self.ctx:here(exit)
   self:loop_leave(save_exit, save_exit_reg)
   self.ctx.freereg = free
end
function StatementRule:BreakStatement()
   if self.exit then
      -- The following call will generate either a JMP instruction or an UCLO instruction
      -- with jump as appropriate.
      self.ctx:close_block(self.exit_reg, self.exit)
   else
      error("no loop to break")
   end
end
function StatementRule:ForStatement(node)
   local free = self.ctx.freereg
   local base = free

   local exit = util.genid()

   local init = node.init
   local name = init.id.name

   self:expr_toreg(init.value, base, 1)
   self.ctx:setreg(base + 1)
   self:expr_toreg(node.last, base + 1, 1)
   self.ctx:setreg(base + 2)
   if node.step then
      self:expr_toreg(node.step, base + 2, 1)
   else
      self.ctx:op_load(base + 2, 1)
   end
   self.ctx:setreg(base + 3)
   local loop = self.ctx:op_fori(base)
   local save_exit, save_exit_reg = self:loop_enter(exit, free)
   self.ctx:newvar(name)
   self:emit(node.body)
   self:loop_leave(save_exit, save_exit_reg)
   self.ctx:op_forl(base, loop)
   self.ctx:here(exit)
   self.ctx.freereg = free
end
function StatementRule:ForInStatement(node)
   local free = self.ctx.freereg
   local iter = free + 3

   local loop, exit = util.genid(), util.genid()

   local vars = node.init.names
   local expr = node.iter

   self:expr_tomultireg(expr, 3) -- func, state, ctl
   self.ctx:nextreg(3)
   self.ctx:jump(loop)

   local save_exit, save_exit_reg = self:loop_enter(exit, free)

   for i=1, #vars do
      local name = vars[i].name
      self.ctx:newvar(name, iter + i - 1)
      self.ctx:setreg(iter + i)
   end

   local ltop = self.ctx:here(util.genid())
   self:emit(node.body)
   self:loop_leave(save_exit, save_exit_reg)
   self.ctx:here(loop)
   self.ctx:op_iterc(iter, #vars)
   self.ctx:op_iterl(iter, ltop)
   self.ctx:here(exit)
   self.ctx.freereg = free
end

function StatementRule:ReturnStatement(node)
   local narg = #node.arguments
   if narg == 0 then
      self.ctx:close_uvals()
      self.ctx:op_ret0()
   elseif narg == 1 then
      local base = self.ctx.freereg
      local dest, tail = self:expr_toanyreg(node.arguments[1], true)
      if not tail then
         self.ctx:close_uvals()
         self.ctx:op_ret1(dest)
      end
   else
      local base = self.ctx.freereg
      for i=1, narg - 1 do
         self:expr_toreg(node.arguments[i], self.ctx.freereg)
         self.ctx:nextreg()
      end
      local lastarg = node.arguments[narg]
      local mret, tail = self:expr_tomultireg(lastarg, MULTIRES, false)
      self.ctx.freereg = base
      if not tail then
         self.ctx:close_uvals()
         if mret then
            self.ctx:op_retm(base, narg - 1)
         else
            self.ctx:op_ret(base, narg)
         end
      end
   end
   if self.ctx:is_root_scope() then
      self.ctx.explret = true
   end
end

function StatementRule:Chunk(tree, name)
   for i=1, #tree.body do
      self:emit(tree.body[i])
   end
   self:close_proto()
end

local function dispatch(self, lookup, node, ...)
   if type(node) ~= "table" then
      error("not a table: "..tostring(node))
   end
   if not node.kind then
      error("don't know what to do with: "..util.dump(node))
   end
   if not lookup[node.kind] then
      error("no handler for "..node.kind)
   end
   return lookup[node.kind](self, node, ...)
end

local function generate(tree, name)
   local self = { line = 0 }
   self.main = bc.Proto.new(bc.Proto.VARARG)
   self.dump = bc.Dump.new(self.main, name)
   self.ctx = self.main

   function self:block_enter()
      self.ctx:enter()
   end

   function self:block_leave(exit)
      self.ctx:close_block(self.ctx.scope.basereg, exit)
      self.ctx:leave()
   end

   function self:loop_enter(exit, exit_reg)
      self:block_enter()
      local prev_exit, prev_exit_reg = self.exit, self.exit_reg
      self.exit, self.exit_reg = exit, exit_reg
      return prev_exit, prev_exit_reg
   end

   function self:loop_leave(exit, exit_reg)
      self:block_leave()
      self.exit, self.exit_reg = exit, exit_reg
   end

   function self:assign(lhs, expr)
      if lhs.tag == 'member' then
         -- SET instructions with a Primitive "P" index are not accepted.
         -- The method self:lhs_expr_emit does never generate such requests.
         assert(lhs.key_type ~= 'P', "invalid assignment instruction")
         self.ctx:op_tset(lhs.target, lhs.key_type, lhs.key, expr)
      elseif lhs.tag == 'upval' then
         self.ctx:op_uset(lhs.uv, 'V', expr)
      elseif lhs.tag == 'local' then
         mov_toreg(self.ctx, lhs.target, expr)
      else
         self.ctx:op_gset(expr, lhs.name)
      end
   end

   function self:emit(node, ...)
      dispatch(self, StatementRule, node, ...)
      if node.line then self.ctx:line(node.line) end
   end

   -- Emit the code to evaluate "node" and perform a conditional
   -- jump based on its value. The argument "jmp" is the jump location.
   -- If "negate" is false the jump on FALSE and viceversa.
   -- The argument "store" is a bitfield that specifies which
   -- computed epxression should be stored. The bit EXPR_RESULT_TRUE
   -- means that the value should be stored when its value is "true".
   -- If "store" is not ZERO than dest should be the register
   -- destination for the result.
   function self:test_emit(node, jmp, negate, store, dest)
      local rule = TestRule[node.kind]
      store = store or 0
      if rule then
         rule(self, node, jmp, negate, store, dest)
      else
         -- LuaJIT compatibility rule: set store to a non zero
         -- value so that the register is counted on test jump
         store, dest = EXPR_RESULT_BOTH, dest or self.ctx.freereg
         self:expr_test(node, jmp, negate, store, dest)
      end
   end

   -- Emit code to test an expression as a boolean value
   function self:expr_test(node, jmp, negate, store, dest)
      local free = self.ctx.freereg
      if dest then
         self:expr_toreg(node, dest)
      else
         dest = self:expr_toanyreg(node)
      end
      local jreg = (store ~= 0 and dest + 1 or free)
      self.ctx:op_test(negate, dest, jmp, jreg)
      self.ctx.freereg = free
   end

   -- Emit code to compute the "node" expression in any register. Return
   -- the register itself and an optional boolean value to indicate if a
   -- tail call was used.
   -- If a new register is needed to store the results one is automatically
   -- allocated and marked as used.
   function self:expr_toanyreg(node, tail)
      local localvar = is_local_var(self.ctx, node)
      if localvar then
         return localvar, false
      else
         local dest = self.ctx.freereg
         local tailcall = self:expr_toreg(node, dest, tail)
         return self.ctx:nextreg(), tailcall
      end
   end

   -- Emit code to compute the "node" expression by storing the result in
   -- the given register "dest". It does return an optional boolean value
   -- to indicate if a tail call was used.
   -- This function always leave the freereg counter to its initial value.
   function self:expr_toreg(node, dest, tail)
      local const_val = self:const_eval_try(node)
      if const_val then
         self.ctx:op_load(dest, const_val)
      else
         local rule = ExpressionRule[node.kind]
         if rule then
            rule(self, node, dest)
         elseif MultiExprRule[node.kind] then
            rule = MultiExprRule[node.kind]
            local base = self.ctx.freereg
            local mres, tailcall = rule(self, node, 1, base == dest and tail)
            mov_toreg(self.ctx, dest, base)
            return tailcall
         else
            error("Cannot find an ExpressionRule for " .. node.kind)
         end
      end
      return false -- no tail call
   end

   -- Generate the code to store multiple values in consecutive registers
   -- starting from the current "freereg". The argument "want" indicate
   -- how many values should be generated or MULTIRES.
   -- The optional boolean parameter "tail" indicate if a tail call instruction
   -- should be generated if possible.
   -- Return two boolean values. The first indicate if it does return multi
   -- results. The second if a tail call was actually generated.
   function self:expr_tomultireg(node, want, tail)
      local rule = MultiExprRule[node.kind]
      if rule then
         return rule(self, node, want, tail)
      elseif (want > 0 or want == MULTIRES) and ExpressionRule[node.kind] then
         rule = ExpressionRule[node.kind]
         local dest = self.ctx.freereg
         rule(self, node, dest)
         if want > 1 then
            self.ctx:op_nils(dest + 1, want - 1)
         end
         return false, false
      end
   end

   -- Like "expr_toreg" but it can return an expression (register) or
   -- an immediate constant. It does return a tag and then the value
   -- itself.
   function self:expr_toanyreg_tagged(node, emit)
      local const_val = self:const_eval_try(node)
      if emit.byte and const_val and is_byte_number(const_val) then
         return 'B', const_val
      elseif emit.number and const_val then
         return 'N', self.ctx:const(const_val)
      end
      if node.kind == 'Literal' then
         local value = node.value
         local tv = type(value)
         if emit.primitive and (tv == 'nil' or tv == 'boolean') then
            return 'P', self.ctx:kpri(value)
         elseif emit.string and tv == 'string' then
            return 'S', self.ctx:const(value)
         end
         -- fall through
      end
      return 'V', self:expr_toanyreg(node)
   end

   function self:lhs_expr_emit(node)
      local rule = assert(LHSExpressionRule[node.kind], "undefined assignment rule for node type: \"" .. node.kind .. "\"")
      return rule(self, node)
   end

   function self:const_eval_try(node)
      local rule = ConstRule[node.kind]
      if rule then
         return rule(self, node)
      end
   end

   function self:close_proto()
      if not self.ctx.explret then
         self.ctx:close_uvals()
         self.ctx:op_ret0()
      end
   end

   self:emit(tree)
   self.ctx:set_line(tree.firstline, tree.lastline)

   return self.dump:pack()
end

return generate
