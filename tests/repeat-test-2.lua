local function foo(n)
   local sum = 0
   local i = 1
   local test
   repeat
      sum = sum + i*i
      i = i + 1
      test = i > n and 1
   until test
   return sum
end

print(foo(20))
