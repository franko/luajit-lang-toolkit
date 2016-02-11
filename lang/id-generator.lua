local function unique_name(used_name_predicate, name)
    if used_name_predicate(name) then
        local prefix, index = string.match(name, "^(.+)(%d+)$")
        if not prefix then
            prefix, index = name, 1
        else
            index = tonumber(index) + 1
        end
        local test_name = prefix .. tostring(index)
        while used_name_predicate(test_name) do
            index = index + 1
            test_name = prefix .. tostring(index)
        end
        return test_name
    else
        return name
    end
end


local function new_scope(parent_scope)
    return {
        vars = { },
        infos = { },
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

local function pseudo(name)
    return '@' .. name
end

local function pseudo_match(pseudo_name)
    return string.match(pseudo_name, "^@(.+)$")
end

-- Create a two pass identifier generator. In the first pass the identifier are in the
-- form "@<name>". Then all the lexical variables should be declared using "var_declare".
-- In the final stage the function "normalize" is used to transform the temporary
-- identifier, like "@foo" into something like "foo" itself or "foo5" if the
-- variable name conflicts with other variables of the same name.
-- All this is to ensure that the
-- * the identifier is a valid identifier string
-- * there are no conflict with other local variables declared in the program
local function create_genid_lexical(create_ident, variables)
    local function match_id_name(id, name) return id.name == name end
    local gen_variables = new_variables_registry(create_ident, match_id_name)

    local function var_is_declared(name)
        return variables:lookup(name) ~= nil
    end

    local function var_is_generated(name)
        return gen_variables:lookup(pseudo(name)) ~= nil
    end

    local function normalize(raw_name)
        local name = pseudo_match(raw_name)
        local uname = unique_name(var_is_declared, name)
        return uname
    end

    local function new_ident(name)
        local uname = unique_name(var_is_generated, name or "_")
        return gen_variables:declare(pseudo(uname))
    end

    local function scope_enter()
        gen_variables:scope_enter()
    end

    local function scope_exit()
        local gen_vars = gen_variables.current.vars
        for i = 1, #gen_vars do
            local id = gen_vars[i]
            id.name = normalize(id.name)
        end
        gen_variables:scope_exit()
    end

    return { new_ident = new_ident, scope_enter = scope_enter, scope_exit = scope_exit }
end

return { create = create_genid_simple, lexical = create_genid_lexical }
