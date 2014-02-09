local foo

local function init(choice)
    if choice == 1 then
        function foo(x)
            return x*x + 1
        end
    else
        function foo(x)
            return x
        end
    end
end

init(1)
print(foo(3))
