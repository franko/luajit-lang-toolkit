
local function undo_h() print("undo_h") end
local function undo_g() print("undo_g") end

function f(x, y, z)
    if x > y then goto fail end
    if x > z then goto cleanup_g end
    if x > y - z then goto cleanup_h end
    do
        print("got it!")
        return true
    end    -- need do/end?

    ::cleanup_h::
    undo_h()
    ::cleanup_g::
    undo_g()
    ::fail::
    return false
end

print(f(3, 10, 20))
print(f(3, 0, 20))
print(f(3, 10, 0))
print(f(3, 10, 10))
