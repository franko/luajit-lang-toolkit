local function foo(x, y)
	if (x < y) and (x*x < y*y) then
		print('boom')
	end
end

local x, y = 3, 7
foo(x, y)
