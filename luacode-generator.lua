--
-- luacode-generator.lua
--
-- This file is part of the LuaJIT Language Toolkit.
--
-- Module to generate the Lua code that corresponds to a given Lua AST Tree.
-- Can be used as an alternative to the bytecode generator.

local operator = require("operator")

local StatementRule = { }
local ExpressionRule = { }

local concat = table.concat
local format = string.format

local function is_string(node)
    return node.kind == "Literal" and type(node.value) == "string"
end

local function is_const(node, val)
    return node.kind == "Literal" and node.value == val
end

local function comma_sep_list(ls, f)
    local strls
    if f then
        strls = { }
        for k = 1, #ls do strls[k] = f(ls[k]) end
    else
        strls = ls
    end
    return concat(strls, ", ")
end

local function as_parameter(node)
    return node.kind == "Vararg" and "..." or node.name
end

function ExpressionRule:Identifier(node)
    return node.name, operator.ident_priority
end

function ExpressionRule:Literal(node)
    local val = node.value
    local str = type(val) == "string" and format("%q", val) or tostring(val)
    return str, operator.ident_priority
end

function ExpressionRule:MemberExpression(node)
    local object, prio = self:expr_emit(node.object)
    if prio < operator.ident_priority then object = "(" .. object .. ")" end
    local exp
    if node.computed then
        local prop = self:expr_emit(node.property)
        exp = format("%s[%s]", object, prop)
    else
        exp = format("%s.%s", object, node.property.name)
    end
    return exp, operator.ident_priority
end

function ExpressionRule:Vararg()
    return "...", operator.ident_priority
end

function ExpressionRule:BinaryExpression(node)
    local oper = node.operator
    local lprio = operator.left_priority(oper)
    local rprio = operator.right_priority(oper)
    local a, alprio, arprio = self:expr_emit(node.left)
    local b, blprio, brprio = self:expr_emit(node.right)
    if not arprio then arprio = alprio end
    if not brprio then brprio = blprio end
    local ap = arprio <  lprio and format("(%s)", a) or a
    local bp = blprio <= rprio and format("(%s)", b) or b
    return format("%s %s %s", ap, oper, bp), lprio, rprio
end

function ExpressionRule:UnaryExpression(node)
    local arg, arg_prio = self:expr_emit(node.argument)
    local op_prio = operator.unary_priority
    if arg_prio < op_prio then arg = format("(%s)", arg) end
    return format("%s%s", node.operator, arg), operator.unary_priority
end

ExpressionRule.LogicalExpression = ExpressionRule.BinaryExpression

function ExpressionRule:ConcatenateExpression(node)
    local ls = {}
    local cat_prio = operator.left_priority("..")
    for k = 1, #node.terms do
        ls[k], kprio = self:expr_emit(node.terms[k])
        if kprio < cat_prio then ls[k] = format("(%s)", ls[k]) end
    end
    return concat(ls, " .. "), cat_prio
end

function ExpressionRule:Table(node)
    local array = self:expr_list(node.array_entries)
    local hash = { }
    for k = 1, #node.hash_keys do
        local key = node.hash_keys[k]
        local value = self:expr_emit(node.hash_values[k])
        if is_string(key) then
            hash[k] = format("%s = %s", key.value, value)
        else
            hash[k] = format("[%s] = %s", self:expr_emit(key), value)
        end
    end
    local content = array
    if #hash > 0 then
        local hash_str = comma_sep_list(hash)
        content = content ~= "" and (content .. ", " .. hash_str) or hash_str
    end
    return "{" .. content .. "}", operator.ident_priority
end

function ExpressionRule:CallExpression(node)
    local callee, prio = self:expr_emit(node.callee)
    if prio < operator.ident_priority then
        callee = "(" .. callee .. ")"
    end
    local exp = format("%s(%s)", callee, self:expr_list(node.arguments))
    return exp, operator.ident_priority
end

function ExpressionRule:SendExpression(node)
    local rec, prio = self:expr_emit(node.receiver)
    if prio < operator.ident_priority then
        rec = "(" .. rec .. ")"
    end
    local method = node.method.name
    local exp = format("%s:%s(%s)", rec, method, self:expr_list(node.arguments))
    return exp, operator.ident_priority
end

function StatementRule:FunctionDeclaration(node)
    self:proto_enter()
    local name = self:expr_emit(node.id)
    local header = format("function %s(%s)", name, comma_sep_list(node.params, as_parameter))
    if node.locald then
        header = "local " .. header
    end
    self:add_section(header, node.body)
    local child_proto = self:proto_leave()
    self.proto:merge(child_proto)
end

function ExpressionRule:FunctionExpression(node)
    self:proto_enter()
    local header = format("function(%s)", comma_sep_list(node.params, as_parameter))
    self:add_section(header, node.body)
    local child_proto = self:proto_leave()
    return child_proto:inline(self.proto.indent), 0
end

function StatementRule:CallExpression(node)
    local line = self:expr_emit(node)
    self:add_line(line)
end

function StatementRule:ForStatement(node)
    local init = node.init
    local istart = self:expr_emit(init.value)
    local iend = self:expr_emit(node.last)
    local header
    if node.step and not is_const(node.step, 1) then
        local step = self:expr_emit(node.step)
        header = format("for %s = %s, %s, %s do", init.id.name, istart, iend, step)
    else
        header = format("for %s = %s, %s do", init.id.name, istart, iend)
    end
    self:add_section(header, node.body)
end

function StatementRule:ForInStatement(node)
    local vars = comma_sep_list(node.namelist.names, as_parameter)
    local explist = self:expr_list(node.explist)
    local header = format("for %s in %s do", vars, explist)
    self:add_section(header, node.body)
end

function StatementRule:DoStatement(node)
    self:add_section("do", node.body)
end

function StatementRule:WhileStatement(node)
    local test = self:expr_emit(node.test)
    local header = format("while %s do", test)
    self:add_section(header, node.body)
end

function StatementRule:RepeatStatement(node)
    self:add_section("repeat", node.body, true)
    local test = self:expr_emit(node.test)
    local until_line = format("until %s", test)
    self:add_line(until_line)
end

function StatementRule:BreakStatement()
    self:add_line("break")
end

function StatementRule:IfStatement(node)
    local ncons = #node.tests
    for i = 1, ncons do
        local header_tag = i == 1 and "if" or "elseif"
        local test = self:expr_emit(node.tests[i])
        local header = format("%s %s then", header_tag, test)
        self:add_section(header, node.cons[i], true)
    end
    if node.alternate then
        self:add_section("else", node.alternate, true)
    end
    self:add_line("end")
end

function StatementRule:LocalDeclaration(node)
    local line
    local names = comma_sep_list(node.names, as_parameter)
    if #node.expressions > 0 then
        line = format("local %s = %s", names, self:expr_list(node.expressions))
    else
        line = format("local %s", names)
    end
    self:add_line(line)
end

function StatementRule:AssignmentExpression(node)
    local line = format("%s = %s", self:expr_list(node.left), self:expr_list(node.right))
    self:add_line(line)
end

function StatementRule:Chunk(node)
    self:list_emit(node.body)
end

function StatementRule:ExpressionStatement(node)
    local line = self:expr_emit(node.expression)
    self:add_line(line)
end

function StatementRule:ReturnStatement(node)
    local line = format("return %s", self:expr_list(node.arguments))
    self:add_line(line)
end

function StatementRule:LabelStatement(node)
   self:add_line("::" .. node.label .. "::")
end

function StatementRule:GotoStatement(node)
   self:add_line("goto " .. node.label)
end

local function proto_inline(proto, indent)
    local ls = { }
    for k = 1, #proto.code do
        local indent_str = k == 1 and "" or string.rep("    ", indent)
        ls[k] = indent_str .. proto.code[k]
    end
    return concat(ls, "\n")
end

local function proto_merge(proto, child)
    for k = 1, #child.code do
        local line = child.code[k]
        local indent_str = string.rep("    ", proto.indent)
        proto.code[#proto.code + 1] = indent_str .. line
    end
end

local function proto_new(parent)
    local proto = { code = { }, indent = 0, parent = parent }
    proto.inline = proto_inline
    proto.merge = proto_merge
    return proto
end

local function generate(tree, name)

    local self = { line = 0 }
    self.proto = proto_new()
    self.chunkname = tree.chunkname

    function self:proto_enter()
        self.proto = proto_new(self.proto)
    end

    function self:proto_leave()
        local proto = self.proto
        self.proto = proto.parent
        return proto
    end

    local function to_expr(node)
        return self:expr_emit(node)
    end

    function self:compile_code()
        return concat(self.code, "\n")
    end

    function self:indent_more()
        local proto = self.proto
        proto.indent = proto.indent + 1
    end

    function self:indent_less()
        local proto = self.proto
        proto.indent = proto.indent - 1
    end

    function self:line(line)
        -- FIXME: ignored for the moment
    end

    function self:add_line(line)
        local proto = self.proto
        local indent = string.rep("    ", proto.indent)
        proto.code[#proto.code + 1] = indent .. line
    end

    function self:add_section(header, body, omit_end)
        self:add_line(header)
        self:indent_more()
        self:list_emit(body)
        self:indent_less()
        if not omit_end then
            self:add_line("end")
        end
    end

    function self:expr_emit(node)
        local rule = ExpressionRule[node.kind]
        return rule(self, node)
    end

    function self:expr_list(exps)
        return comma_sep_list(exps, to_expr)
    end

    function self:emit(node)
        local rule = StatementRule[node.kind]
          if not rule then error("cannot find a statement rule for " .. node.kind) end
          rule(self, node)
          if node.line then self:line(node.line) end
    end

    function self:list_emit(node_list)
        for i = 1, #node_list do
            self:emit(node_list[i])
        end
    end

    self:emit(tree)

    return self:proto_leave():inline(0)
end

return generate
