local a = (7 % 2 == 1)
local b = (8 % 2 == 1)
local c = (9 % 2 == 1)

if not a then print("a") else print("b") end
if not a and 10 then print("a") else print("b") end
if not a or  10 then print("a") else print("b") end

if not b then print("a") else print("b") end
if not b and 10 then print("a") else print("b") end
if not b or  10 then print("a") else print("b") end

if (not a) and b or c then print("a") else print("b") end
if (not a) and (not b or c) or c then print("a") else print("b") end
if (not a or c) and not b or c then print("a") else print("b") end

if (not b) and a or b then print("a") else print("b") end
if (not b) and (not c or b) or b then print("a") else print("b") end
if (not b or a) and not c or b then print("a") else print("b") end
