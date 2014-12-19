local function foo(...)
    local arg = {..., "friend"}
    for k = 1, #arg do
        print(arg[k])
    end
end

foo("hello", "boy")

