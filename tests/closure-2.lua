local function make(n)
	return {
		incr = function(i) n = n + i end,
		mult = function(i) n = n * i end,
		get  = function() return n end,
	}
end

local obj = make(7)
obj.mult(3)
obj.incr(1)
obj.mult(2)
obj.incr(5)
print(obj.get())
