-- Create a two pass identifier generator. In the first pass the identifier are in the
-- form "@<number>". Then all the lexical variables should be declared using "var_declare".
-- In the final stage the function "normalize" is used to transform the temporary
-- identifier, like "@2" into something like "__12". All this is to ensure that the
-- * the identifier is a valid identifier string
-- * there are no conflict with other local variables declared in the program
local function create_genid_lexical(create_ident)
    local intervals = { {1, 2^32 - 1} }
    local longest = 1
    local current = 0

    local pending_idents = {}

    local function find_longest()
        local ilong, isize = 1, -1
        for i = 1, #intervals do
            local size = intervals[i][2] - intervals[i][1]
            if size > isize then
                ilong, isize = i, size
            end
        end
        longest = ilong
    end

    local function remove_id(n)
        for i = 1, #intervals do
            local a, b = intervals[i][1], intervals[i][2]
            if a <= n and n <= b then
                table.remove(intervals, i)
                if n > a then table.insert(intervals, i, {a, n - 1}) end
                if n < b then table.insert(intervals, i, {n + 1, b}) end
                if longest >= i then find_longest() end
                break
            end
        end
    end

    local function var_declare(name)
        local idn = string.match(name, "^__(%d+)$")
        if idn then
            remove_id(tonumber(idn))
        end
    end

    local function normal_name(n)
        local name = intervals[longest][1] + (n - 1)
        assert(name <= intervals[longest][2], "cannot generate new identifier")
        return "__" .. name
    end

    local function normalize(raw_name)
        local n = tonumber(string.match(raw_name, "^@(%d+)$"))
        return normal_name(n)
    end

    local function new_ident()
        current = current + 1
        local id
        if pending_idents then -- Create a temporary name.
            id = create_ident("@" .. current)
            pending_idents[#pending_idents+1] = id
        else -- Generate the final name.
            local name = normal_name(current)
            id = create_ident(name)
        end
        return id
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
