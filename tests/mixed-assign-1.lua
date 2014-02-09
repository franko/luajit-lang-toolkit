local x = {}
for k = 1, 10 do
    k, x[k] = k + 1, k*k + 1
end
print(x[1], x[2], x[3])
