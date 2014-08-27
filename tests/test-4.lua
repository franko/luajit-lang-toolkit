local k = "foo"
function some_f(s) return string.byte(s) end
k = tonumber(k) or some_f(k) or k
print("K", k)

