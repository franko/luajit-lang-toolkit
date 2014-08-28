local foo = function()
    return 4
end

for i = 1, foo() do
    print("A", i)
end

for i = 1, 4 do
    print("B", i)
end
