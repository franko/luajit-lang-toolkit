local function foo(n)
   local sum = 0
   local i = 1
   repeat
      sum = sum + i*i
      i = i + 1
   until i > n
   return sum
end

print(foo(20))
