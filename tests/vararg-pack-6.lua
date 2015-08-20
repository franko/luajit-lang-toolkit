local foo = function(...)
   local arg = {n=select('#',...),...}
   -- arg.n is the real size
   for i = 1,arg.n do
     print(arg[i])
   end
end

foo("hello", nil, nil, "boy", 3, 4)

