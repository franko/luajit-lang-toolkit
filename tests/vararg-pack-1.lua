local function foo(...)
    local arg = {...}
    for k = 1, #arg do
        print(arg[k])
    end
end

foo("hello", "boy")

