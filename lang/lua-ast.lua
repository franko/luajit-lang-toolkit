local id_generator = require("lang.id-generator")

local function build(kind, node)
    node.kind = kind
    return node
end

local function ident(name, line)
    return build("Identifier", { name = name, line = line })
end

local function literal(value, line)
    return build("Literal", { value = value, line = line })
end

local function field(obj, name, line)
    return build("MemberExpression", { object = obj, property = ident(name), computed = false, line = line })
end

local function logical_binop(op, left, right, line)
    return build("LogicalExpression", { operator = op, left = left, right = right, line = line })
end

local function binop(op, left, right, line)
    return build("BinaryExpression", { operator = op, left = left, right = right, line = line })
end

local function empty_table(line)
    return build("Table", { keyvals = { }, line = line })
end

local function does_multi_return(expr)
    local k = expr.kind
    return k == "CallExpression" or k == "SendExpression" or k == "Vararg"
end

local AST = { }

local function func_decl(id, body, params, vararg, locald, firstline, lastline)
    return build("FunctionDeclaration", {
        id         = id,
        body       = body,
        params     = params,
        vararg     = vararg,
        locald     = locald,
        firstline  = firstline,
        lastline   = lastline,
        line       = firstline,
    })
end

local function func_expr(body, params, vararg, firstline, lastline)
    return build("FunctionExpression", { body = body, params = params, vararg = vararg, firstline = firstline, lastline = lastline })
end

function AST.expr_function(ast, args, body, proto)
   return func_expr(body, args, proto.varargs, proto.firstline, proto.lastline)
end

function AST.local_function_decl(ast, name, args, body, proto)
    local id = ast:var_declare(name)
    return func_decl(id, body, args, proto.varargs, true, proto.firstline, proto.lastline)
end

function AST.function_decl(ast, path, args, body, proto)
   return func_decl(path, body, args, proto.varargs, false, proto.firstline, proto.lastline)
end

function AST.func_parameters_decl(ast, args, vararg)
    local params = {}
    for i = 1, #args do
        params[i] = ast:var_declare(args[i])
    end
    if vararg then
        params[#params + 1] = ast:expr_vararg()
    end
    return params
end

function AST.chunk(ast, body, chunkname, firstline, lastline)
    return build("Chunk", { body = body, chunkname = chunkname, firstline = firstline, lastline = lastline })
end

function AST.local_decl(ast, vlist, exps, line)
    local ids = {}
    for k = 1, #vlist do
        ids[k] = ast:var_declare(vlist[k])
    end
    return build("LocalDeclaration", { names = ids, expressions = exps, line = line })
end

function AST.assignment_expr(ast, vars, exps, line)
    return build("AssignmentExpression", { left = vars, right = exps, line = line })
end

function AST.expr_index(ast, v, index, line)
    return build("MemberExpression", { object = v, property = index, computed = true, line = line })
end

function AST.expr_property(ast, v, prop, line)
    local index = ident(prop, line)
    return build("MemberExpression", { object = v, property = index, computed = false, line = line })
end

function AST.literal(ast, val)
    return build("Literal", { value = val })
end

function AST.expr_vararg(ast)
    return build("Vararg", { })
end

function AST.expr_brackets(ast, expr)
    expr.bracketed = true
    return expr
end

function AST.set_expr_last(ast, expr)
    if expr.bracketed and does_multi_return(expr) then
        expr.bracketed = nil
        return build("ExpressionValue", { value = expr })
    else
        return expr
    end
end

function AST.expr_table(ast, keyvals, line)
    return build("Table", { keyvals = keyvals, line = line })
end

function AST.expr_unop(ast, op, v, line)
    return build("UnaryExpression", { operator = op, argument = v, line = line })
end

local function concat_append(ts, node)
    local n = #ts
    if node.kind == "ConcatenateExpression" then
        for k = 1, #node.terms do ts[n + k] = node.terms[k] end
    else
        ts[n + 1] = node
    end
end

function AST.expr_binop(ast, op, expa, expb, line)
    local binop_body = (op ~= '..' and { operator = op, left = expa, right = expb, line = line })
    if binop_body then
        if op == 'and' or op == 'or' then
            return build("LogicalExpression", binop_body)
        else
            return build("BinaryExpression", binop_body)
        end
    else
        local terms = { }
        concat_append(terms, expa)
        concat_append(terms, expb)
        return build("ConcatenateExpression", { terms = terms, line = expa.line })
    end
end

function AST.identifier(ast, name)
    return ident(name)
end

function AST.expr_method_call(ast, v, key, args, line)
    local m = ident(key)
    return build("SendExpression", { receiver = v, method = m, arguments = args, line = line })
end

function AST.expr_function_call(ast, v, args, line)
    return build("CallExpression", { callee = v, arguments = args, line = line })
end

function AST.return_stmt(ast, exps, line)
    return build("ReturnStatement", { arguments = exps, line = line })
end

function AST.break_stmt(ast, line)
    return build("BreakStatement", { line = line })
end

function AST.label_stmt(ast, name, line)
    return build("LabelStatement", { label = name, line = line })
end

function AST.new_statement_expr(ast, expr, line)
    return build("ExpressionStatement", { expression = expr, line = line })
end

function AST.if_stmt(ast, tests, cons, else_branch, line)
    return build("IfStatement", { tests = tests, cons = cons, alternate = else_branch, line = line })
end

function AST.do_stmt(ast, body, line, lastline)
    return build("DoStatement", { body = body, line = line, lastline = lastline})
end

function AST.while_stmt(ast, test, body, line, lastline)
    return build("WhileStatement", { test = test, body = body, line = line, lastline = lastline })
end

function AST.repeat_stmt(ast, test, body, line, lastline)
    return build("RepeatStatement", { test = test, body = body, line = line, lastline = lastline })
end

function AST.for_stmt(ast, var, init, last, step, body, line, lastline)
    local for_init = build("ForInit", { id = var, value = init, line = line })
    return build("ForStatement", { init = for_init, last = last, step = step, body = body, line = line, lastline = lastline })
end

function AST.for_iter_stmt(ast, vars, exps, body, line, lastline)
    local names = build("ForNames", { names = vars, line = line })
    return build("ForInStatement", { namelist = names, explist = exps, body = body, line = line, lastline = lastline })
end

function AST.goto_stmt(ast, name, line)
    return build("GotoStatement", { label = name, line = line })
end

function AST.var_declare(ast, name)
    local id = ident(name)
    ast.variables:declare(name)
    return id
end

function AST.genid(ast, name)
    return id_generator.genid(ast.variables, name)
end

function AST.fscope_begin(ast)
    ast.variables:scope_enter()
end

function AST.fscope_end(ast)
    -- It is important to call id_generator.close_gen_variables before
    -- leaving the "variables" scope.
    id_generator.close_gen_variables(ast.variables)
    ast.variables:scope_exit()
end

local ASTClass = { __index = AST }

local function new_scope(parent_scope)
    return {
        vars = { },
        parent = parent_scope,
    }
end

local function new_variables_registry(create, match)
    local declare = function(self, name)
        local vars = self.current.vars
        local entry = create(name)
        vars[#vars+1] = entry
        return entry
    end

    local scope_enter = function(self)
        self.current = new_scope(self.current)
    end

    local scope_exit = function(self)
        self.current = self.current.parent
    end

    local lookup = function(self, name)
        local scope = self.current
        while scope do
            for i = 1, #scope.vars do
                if match(scope.vars[i], name) then
                    return scope
                end
            end
            scope = scope.parent
        end
    end

    return { declare = declare, scope_enter = scope_enter, scope_exit = scope_exit, lookup = lookup }
end

local function new_ast()
    local match_id_name = function(id, name) return id.name == name end
    local vars = new_variables_registry(ident, match_id_name)
    return setmetatable({ variables = vars }, ASTClass)
end

return { New = new_ast }
