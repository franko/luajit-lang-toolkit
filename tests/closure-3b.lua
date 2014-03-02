
local function foo(n)
	local f
	local s = 0
	for k = 1, n do
		local ksq, kc, kq = k*k, k^3, k^4
		if k % 7 == 0 then
			local kq = k/7 + 1
			f = function(x) return x + k end
			return f
		end
		local kadd = kc - kq
		s = s + kadd
	end
	return f
end

local f = foo(10)
print(f(3))
