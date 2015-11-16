-- Priorities for each binary operator.
-- (left priority) * 256 + (right priority)
-- modulus is your friend
local binop = {
    ['+']  = 6 * 256 + 6, ['-']  = 6 * 256 + 6, ['*'] = 7 * 256 + 7, ['/'] = 7 * 256 + 7, ['%'] = 7 * 256 + 7,
    ['^']  = 10* 256 + 9, ['..'] = 5 * 256 + 4, -- POW CONCAT (right associative)
    ['=='] = 3 * 256 + 3, ['~='] = 3 * 256 + 3,
    ['<']  = 3 * 256 + 3, ['>='] = 3 * 256 + 3, ['>'] = 3 * 256 + 3, ['<='] = 3 * 256 + 3,
    ['and']= 2 * 256 + 2, ['or'] = 1 * 256 + 1,
}

local unary_priority = 8

-- Pseudo priority of a simple identifier. Should be higher than any
-- others operator's priority.
local ident_priority = 16

local function is_binop(op)
    return binop[op]
end

local function left_priority(op)
    return bit.rshift(binop[op], 8)
end

local function right_priority(op)
    return bit.band(binop[op], 0xff)
end

return {
    is_binop       = is_binop,
    left_priority  = left_priority,
    right_priority = right_priority,
    unary_priority = unary_priority,
    ident_priority = ident_priority,
}
