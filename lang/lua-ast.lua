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

local function list_extend(ls, src)
    local n = #ls
    for i = 1, #src do ls[n+i] = src[i] end
    return ls
end

local function lookup_key_pair_node_list(kwargs, property)
    for i = 1, #kwargs do
        local kwname = kwargs[i][2].value
        if kwname == property then
            return kwargs[i][1]
        end
    end
end

local function build_option_from_node(table_node, property, default_expr_node_list, line)
    local index_table = field(table_node, property, line)
    local expr_first_clause = logical_binop("and", binop("~=", index_table, literal(nil), line), index_table, line)
    return logical_binop("or", expr_first_clause, lookup_key_pair_node_list(default_expr_node_list, property), line)
end

local function keywords_func_helper_stmts(ast, body, args, kwargs, vararg, firstline, lastline)
    local kwargs_id, nakedf_id, object_id, fallbf_id = ast:genid(), ast:genid(), ast:genid(), ast:genid()

    local naked_func_args = list_extend({ kwargs_id }, args)

    local kwids, kwvalues = {}, {}
    for i = 1, #kwargs do
        local kwname = kwargs[i][2].value
        kwids[i] = ident(kwname)
        kwvalues[i] = build_option_from_node(kwargs_id, kwname, kwargs, firstline)
    end
    local kw_vars_local_decl = build("LocalDeclaration", { names = kwids, expressions = kwvalues, line = firstline })

    local naked_func_body = list_extend({ kw_vars_local_decl}, body)
    local naked_func = func_decl(nakedf_id, naked_func_body, naked_func_args, vararg, true, firstline, lastline)

    local fallback_args = list_extend({ object_id }, args)
    local fallback_call_params = list_extend({ empty_table(firstline) }, args)
    local fallback_call = build("CallExpression", { callee = nakedf_id, arguments = fallback_call_params, line = firstline })
    local fallback_func_body = { build("ReturnStatement", { arguments = { fallback_call }, line = firstline }) }
    local fallback_func = func_decl(fallbf_id, fallback_func_body, fallback_args, false, true, firstline, lastline)

    local obj_table = build("Table", { keyvals = { { nakedf_id, literal("__kwcall") } }, line = firstline })
    local obj_meta = build("Table", { keyvals = { { fallbf_id, literal("__call") } }, line = firstline })
    local create_obj_call = build("CallExpression", { callee = ident("setmetatable"), arguments = { obj_table, obj_meta }, line = firstline })

    return naked_func, fallback_func, create_obj_call
end

local function func_decl_keywords(ast, id, body, args, kwargs, vararg, locald, firstline, lastline)
    local func_decl = build("LocalDeclaration", { names = { id } , expressions = { }, line = firstline })
    local naked_func, fallback_func, create_obj_call = keywords_func_helper_stmts(ast, body, args, kwargs, vararg, firstline, lastline)
    local obj_assign = build("AssignmentExpression", { left = { id }, right = { create_obj_call }, line = firstline })
    local gen_stmts = locald and { func_decl, naked_func, fallback_func, obj_assign } or { naked_func, fallback_func, obj_assign }
    return build("StatementsGroup", { statements = gen_stmts, line = firstline })
end

local function func_expr_keywords(ast, body, args, kwargs, vararg, line)
    local naked_func, fallback_func, create_obj_call = keywords_func_helper_stmts(ast, body, args, kwargs, vararg, line, line)
    return build("StatementsBlockExpression", { statements = { naked_func, fallback_func }, expr = create_obj_call, line = line })
end

function AST.expr_function(ast, args, body, proto)
    if args.kwargs then
        return func_expr_keywords(ast, body, args, args.kwargs, proto.varargs, proto.firstline)
    else
        return func_expr(body, args, proto.varargs, proto.firstline, proto.lastline)
    end
end

function AST.local_function_decl(ast, name, args, body, proto)
    local id = ast:var_declare(name)
    if args.kwargs then
        return func_decl_keywords(ast, id, body, args, args.kwargs, proto.varargs, true, proto.firstline, proto.lastline)
    else
        return func_decl(id, body, args, proto.varargs, true, proto.firstline, proto.lastline)
    end
end

function AST.function_decl(ast, path, args, body, proto)
    if args.kwargs then
        return func_decl_keywords(ast, path, body, args, args.kwargs, proto.varargs, false, proto.firstline, proto.lastline)
    else
        return func_decl(path, body, args, proto.varargs, false, proto.firstline, proto.lastline)
    end
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

function AST.expr_function_call(ast, v, args, kwargs, line)
    if kwargs then
        local kwt = build("Table", { keyvals = kwargs, line = line })
        local ext_args = { kwt }
        for i = 1, #args do
            ext_args[i+1] = args[i]
        end
        local kw_callee = field(v, "__kwcall", line)
        return build("CallExpression", { callee = kw_callee, arguments = ext_args, line = line })
    else
        return build("CallExpression", { callee = v, arguments = args, line = line })
    end
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

local function new_scope(parent_scope)
    return {
        vars = { },
        parent = parent_scope,
    }
end

function AST.var_declare(ast, name)
    local id = ident(name)
    ast.current.vars[name] = true
    ast.id_generator.var_declare(name)
    return id
end

function AST.genid(ast)
    return ast.id_generator.new_ident()
end

function AST.fscope_begin(ast)
    ast.current = new_scope(ast.current)
end

function AST.fscope_end(ast)
    ast.current = ast.current.parent
end

function AST.close(ast)
    ast.id_generator.close_lexical()
end

local ASTClass = { __index = AST }

local function new_ast(genid)
    return setmetatable({ id_generator = genid }, ASTClass)
end

return { New = new_ast }
