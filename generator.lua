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

local function is_byte_number(v)
   return type(v) == 'number' and v % 1 == 0 and v >= 0 and v < 256
end

function ExpressionRule:Literal(node, dest)
   dest = dest or self.ctx:nextreg()
   self.ctx:op_load(dest, node.value)
   return dest
end

function ExpressionRule:Identifier(node, dest, want)
   want = want or 0
   local info, uval = self.ctx:lookup(node.name)
   if info then
      if uval then
         dest = dest or self.ctx:nextreg()
         self.ctx:op_uget(dest, node.name)
      else
         local var = self.ctx:lookup(node.name)
         dest = dest or var.idx
         if dest ~= var.idx then
             self.ctx:op_move(dest, var.idx)
         end
      end
   else
      dest = dest or self.ctx:nextreg()
      self.ctx:op_gget(dest, node.name)
   end
   return dest
end

function ExpressionRule:Vararg(node, base, want)
   if want > 1 and not base then error("Vararg needs a base") end
   base = base or self.ctx:nextreg()
   self.ctx:op_varg(base, want)
   return base, true
end

function ExpressionRule:Table(node, dest)
   local free
   if dest then
      free = self.ctx.freereg
      if dest + 1 > free then self.ctx:setreg(dest + 1) end
   else
      dest = self.ctx:nextreg()
      free = self.ctx.freereg
   end
   self.ctx:op_tnew(dest)
   local vtop = self.ctx.freereg
   for k = 1, #node.array_entries do
      local ktag, kval
      if k < 256 then
         ktag, kval = 'B', k
      else
         ktag, kval = 'V', self.ctx:nextreg()
         self.ctx:op_load(kval, k)
      end
      local v = self:expr_emit(node.array_entries[k])
      self.ctx:op_tset(dest, ktag, kval, v)
      self.ctx.freereg = vtop
   end

   for i = 1, #node.hash_keys do
      local key, value = node.hash_keys[i], node.hash_values[i]
      local ktag, kval = self:expr_emit_tagged(key, EXPR_EMIT_VSB)
      local v = self:expr_emit(value)
      self.ctx:op_tset(dest, ktag, kval, v)
      self.ctx.freereg = vtop
   end

   self.ctx.freereg = free
   return dest
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
      self:expr_emit(node.terms[i + 1], free + i)
      self.ctx:setreg(free + i + 1)
   end
   self.ctx.freereg = free
   dest = dest or self.ctx:nextreg()
   self.ctx:op_cat(dest, free, free + #node.terms - 1)
   return dest
end

function ExpressionRule:BinaryExpression(node, dest)
   local o = node.operator
   if cmpop[o] then
      local l = util.genid()
      local result = dest or self.ctx.freereg
      self:test_emit(node, l, false, EXPR_RESULT_BOTH, result)
      dest = dest or self.ctx:nextreg()
      self.ctx:here(l)
   elseif dirop[o] then
      local free = self.ctx.freereg
      local atag, a = self:expr_emit_tagged(node.left, EXPR_EMIT_VN)
      local btag, b = self:expr_emit_tagged(node.right, EXPR_EMIT_VN)
      self.ctx.freereg = free
      dest = dest or self.ctx:nextreg()
      assert(not (atag == 'N' and btag == 'N'), "operands are both constants")
      self.ctx:op_infix(dirop[o], dest, atag, a, btag, b)
   else
      local free = self.ctx.freereg
      local a = self:expr_emit(node.left)
      local b = self:expr_emit(node.right)
      self.ctx.freereg = free
      dest = dest or self.ctx:nextreg()
      if o == '^' then
         self.ctx:op_pow(dest, a, b)
      else
         error("bad binary operator: "..o, 2)
      end
   end
   return dest
end

function ExpressionRule:UnaryExpression(node, dest)
   local o = node.operator
   local free = self.ctx.freereg
   if o == "'" then
      local base = self.ctx:nextreg()
      self.ctx:op_gget(base, "matrix")
      self.ctx:op_tget(base, base, "S", self.ctx:const("transpose"))
      self:expr_emit(node.argument, base + 1)
      self.ctx:op_call(base, 1, 1)
      if dest then
         if base ~= dest then
            self.ctx:op_move(dest, base)
         end
         self.ctx.freereg = free
      end
      return base
   else
      local a = self:expr_emit(node.argument)
      self.ctx.freereg = free
      dest = dest or self.ctx:nextreg()
      if o == '-' then
         self.ctx:op_unm(dest, a)
      elseif o == '#' then
         self.ctx:op_len(dest, a)
      elseif o == 'not' then
         self.ctx:op_not(dest, a)
      else
         error("bad unary operator: "..o, 2)
      end
      return dest
   end
end

function ExpressionRule:LogicalExpression(node, dest)
   local result = dest or self.ctx.freereg
   local negate = (node.operator == 'or')
   local lstore = (node.operator == 'or' and EXPR_RESULT_TRUE or EXPR_RESULT_FALSE)
   local l = util.genid()
   self:test_emit(node.left, l, negate, lstore, result)
   self:expr_emit(node.right, result)
   self.ctx:here(l)
   if not dest then self.ctx:nextreg() end
   return result
end

function ExpressionRule:MemberExpression(node, base)
   local free = self.ctx.freereg
   local lhs = self:lhs_expr_emit(node)
   self.ctx.freereg = free
   base = base or self.ctx:nextreg()
   self.ctx:op_tget(base, lhs.target, lhs.key_type, lhs.key)
   return base
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

   self.ctx = self.ctx.outer
   self.ctx.freereg = free
   dest = dest or self.ctx:nextreg()
   self.ctx:op_fnew(dest, func.idx)

   return dest
end

local function emit_call_expression(self, node, dest, want, tail, use_self)
   local base = self.ctx.freereg
   local free = (not dest and want == 1) and base + 1 or base

   if use_self then
      local obj = self:expr_emit(node.receiver)
      self.ctx:setreg(base + 1)
      self.ctx:op_move(base + 1, obj)
      local method = self.ctx:const(node.method.name)
      self.ctx:op_tget(base, obj, 'S', method)
      self.ctx:nextreg()
   else
      self:expr_emit(node.callee, base)
      self.ctx:nextreg()
   end

   local narg = #node.arguments
   for i=1, narg - 1 do
      self:expr_emit(node.arguments[i], self.ctx.freereg)
      self.ctx:nextreg()
   end
   local mres = false
   if narg > 0 then
      local lastarg = node.arguments[narg]
      local ret
      ret, mres = self:expr_emit(lastarg, self.ctx.freereg, MULTIRES)
   end

   dest = dest or base
   local use_tail = tail and (base == dest)

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

   if dest ~= base then
      assert(want == 1, "CallExpression cannot return multiple values into inner register")
      self.ctx:op_move(dest, base)
   end

   return dest, want == MULTIRES, use_tail
end

function ExpressionRule:CallExpression(node, dest, want, tail)
   return emit_call_expression(self, node, dest, want, tail, false)
end

function ExpressionRule:SendExpression(node, dest, want, tail)
   return emit_call_expression(self, node, dest, want, tail, true)
end

function LHSExpressionRule:Identifier(node)
   local info, uval = self.ctx:lookup(node.name)
   if uval then
      self.ctx:upval(node.name)
      return {tag = 'upval', name = node.name}
   elseif info then
      return {tag = 'local', target = info.idx}
   else
      return {tag = 'global', name = node.name}
   end
end

function LHSExpressionRule:MemberExpression(node)
   local target = self:expr_emit(node.object)
   local key_type, key
   if node.computed then
      key_type, key = self:expr_emit_tagged(node.property, EXPR_EMIT_VSB)
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
      self:expr_emit(node, dest)
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
         atag, a = self:expr_emit_tagged(node.left, EXPR_EMIT_VSNP)
         if atag == 'V' then
            btag, b = self:expr_emit_tagged(node.right, EXPR_EMIT_VSNP)
         else
            btag, b = atag, a
            atag, a = 'V', self:expr_emit(node.right)
         end
      else
         a = self:expr_emit(node.left)
         b = self:expr_emit(node.right)
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

local multi_returns = {
   CallExpression = true,
   SendExpression = true,
   Vararg = true,
}

local pure_exprs = {
   Literal = true,
   Vararg = true,
   Table = true,
}

local function is_pure_exprs(node)
   return pure_exprs[node.kind]
end

local function can_multi_return(node)
   return multi_returns[node.kind]
end

function StatementRule:CallExpression(node)
   self:expr_emit(node, nil, 0)
end

function StatementRule:SendExpression(node)
   self:expr_emit(node, nil, 0)
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
   for i=1, nexps do
      if slots > 0 then
         local w = (i == nexps and slots or 1)
         local dest = base + (i - 1)
         self:expr_emit(node.expressions[i], dest, w)
         self.ctx:setreg(dest + w)
         slots = slots - w
      else
         self:expr_emit(node.expressions[i], nil, 0)
      end
   end

   if slots > 0 then
      self.ctx:op_nils(base + nexps, slots)
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
      if slots > 0 then
         -- LuaJIT compatibility:
         -- Use a temporary register even the LHS is not an immediate local
         -- variable.
         local use_reg = true
         --[[
         local use_reg = is_local_var(self.ctx, node.left[i])
         ]]
         local reg = use_reg and self.ctx.freereg
         exprs[i] = self:expr_emit(node.right[i], reg)
         if use_reg then self.ctx:nextreg() end
         slots = slots - 1
      else
         self:expr_emit(node.right[i], nil, 0)
      end
   end

   local i = nexps
   if slots == 0 then
      self:expr_emit(node.right[i], nil, 0)
   elseif slots == 1 then
      if lhs[i].tag == 'upval' then
         local tag, expr = self:expr_emit_tagged(node.right[i], EXPR_EMIT_VSNP)
         self.ctx:op_uset(lhs[i].name, tag, expr)
         nvars = nvars - 1
      elseif lhs[i].tag == 'local' then
         self:expr_emit(node.right[i], lhs[i].target)
         nvars = nvars - 1
      else
         exprs[i] = self:expr_emit(node.right[i])
      end
   else
      local exp_base = self:expr_emit(node.right[i], self.ctx.freereg, slots)
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

function StatementRule:FunctionDeclaration(node)
   local name = node.id.name
   local dest
   if node.locald then
      dest = self.ctx:newvar(name).idx
   else
      dest = self.ctx.freereg
   end

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

   self.ctx = self.ctx.outer
   self.ctx.freereg = free
   self.ctx:op_fnew(dest, func.idx)
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
      self.ctx:close_block_uvals(self.exit_reg, self.exit)
      self.ctx.scope.uvclosed = true
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

   self:expr_emit(init.value, base, 1)
   self.ctx:setreg(base + 1)
   self:expr_emit(node.last, base + 1, 1)
   self.ctx:setreg(base + 2)
   if node.step then
      self:expr_emit(node.step, base + 2, 1)
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
   local base, iter = free, free + 3

   local loop, exit = util.genid(), util.genid()

   local vars = node.init.names
   local expr = node.iter

   self:expr_emit(expr, base, 3) -- func, state, ctl
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
      local dest, _, tail = self:expr_emit(node.arguments[1], nil, 1, true)
      if not tail then
         self.ctx:close_uvals()
         self.ctx:op_ret1(dest)
      end
   else
      local base = self.ctx.freereg
      local current = base
      for i=1, narg - 1 do
         self:expr_emit(node.arguments[i], current, 1)
         self.ctx:nextreg()
         current = current + 1
      end
      local lastarg = node.arguments[narg]
      local _, mret, tail = self:expr_emit(lastarg, current, MULTIRES)
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
   self.ctx.scope.uvclosed = true
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
      self.ctx:close_block_uvals(self.ctx.scope.basereg, exit)
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
         self.ctx:op_uset(lhs.name, 'V', expr)
      elseif lhs.tag == 'local' then
         local dest = lhs.target
         if dest ~= expr then
            self.ctx:op_move(dest, expr)
         end
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
      local expr = self:expr_emit(node, dest)
      local jreg = (store ~= 0 and dest + 1 or free)
      self.ctx:op_test(negate, expr, jmp, jreg)
      self.ctx.freereg = free
   end

   function self:expr_emit(node, base, want, tail)
      want = want or 1
      if want == 0 and is_pure_exprs(node) then return end
      if can_multi_return(node) then
         return dispatch(self, ExpressionRule, node, base, want, tail)
      else
         local const_val = self:const_eval_try(node)
         local dest
         if const_val and (want == 1 or want == MULTIRES) then
            dest = base or self.ctx:nextreg()
            self.ctx:op_load(dest, const_val)
         else
            dest = dispatch(self, ExpressionRule, node, base)
         end
         if want > 1 then
            self.ctx:op_nils(base + 1, want - 1)
         end
         return dest, false
      end
   end

   -- Like "expr_emit" but it can return an expression (register) or
   -- an immediate constant. It does return a tag and then the value
   -- itself.
   function self:expr_emit_tagged(node, emit)
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
      return 'V', self:expr_emit(node)
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
      self.ctx:close()
   end

   self:emit(tree)
   return self.dump:pack()
end

return generate
