local l = { }

for x=1, 5 do ::redo::
	local y = x^2 + 1
	if x < 30 then
		l[#l+1] = function() return y end
		x = y
		goto redo
	end
end

for k = 1, #l do
	print(l[k]())
end