local function foo(msg, ...)
    local arg = {"wait", "a", "moment", ...}
    print(msg)
    for k = 1, #arg do
        print(arg[k])
    end
end

foo("Hey", "hello", "boy")

