local eq = function(n, t1, t2)
   for i = 1, n do
      if t1[i] ~= t2[i] then
         local msg = "[" .. i .. "]: "
         msg = msg .. (tostring(t1[i]) or "nil") .. " " .. (tostring(t2[i]) or "nil")
         error(msg, 2)
      end
   end
end 
local foo = function(...)
   return ...
end
local triple, single = {2, 3, 4}, {2, nil, nil}
eq(4, {foo(2,3,4)}, triple)
eq(4, {(foo(2,3,4))}, single)
eq(6, {foo(2,3,4), 3, 4}, triple)
eq(6, {2, 3, (foo(4,5,6))}, triple)
eq(6, {2, 3, foo(4,5,6)}, {2, 3, 4, 5, 6})
