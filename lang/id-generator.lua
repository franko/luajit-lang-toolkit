local function unique_name(used_names, name)
    if used_names[name] then
        local prefix, index = string.match(name, "^(.+)(%d+)$")
        if not prefix then
            prefix, index = name, 1
        else
            index = tonumber(index) + 1
        end
        local test_name = prefix .. tostring(index)
        while used_names[test_name] do
            index = index + 1
            test_name = prefix .. tostring(index)
        end
        return test_name
    else
        return name
    end
end

-- Create a two pass identifier generator. In the first pass the identifier are in the
-- form "@<name>". Then all the lexical variables should be declared using "var_declare".
-- In the final stage the function "normalize" is used to transform the temporary
-- identifier, like "@foo" into something like "foo" itself or "foo5" if the
-- variable name conflicts with other variables of the same name.
-- All this is to ensure that the
-- * the identifier is a valid identifier string
-- * there are no conflict with other local variables declared in the program
local function create_genid_lexical(create_ident)
    local used_names = { }
    local gen_names = { }
    local anon_index = 1

    local pending_idents = { }

    local function var_declare(name)
        used_names[name] = true
    end

    local function normalize(raw_name)
        local name = string.match(raw_name, "^@(.+)$")
        local uname = unique_name(used_names, name)
        used_names[uname] = true
        return uname
    end

    local function new_ident(name)
        local uname = unique_name(gen_names, name or "_")
        gen_names[uname] = true
        if pending_idents then -- Create a temporary name.
            local id = create_ident("@" .. uname)
            pending_idents[#pending_idents+1] = id
            return id
        else -- Generate the final name.
            error("Requesting generated identifier after close_lexical()")
        end
    end

    -- When called this means that all the lexical variables have been
    -- declared with "var_declare".
    local function close_lexical()
        for i = 1, #pending_idents do
            local id = pending_idents[i]
            id.name = normalize(id.name)
        end
        pending_idents = nil
    end

    return { new_ident = new_ident, var_declare = var_declare, close_lexical = close_lexical }
end

return { create = create_genid_simple, lexical = create_genid_lexical }
