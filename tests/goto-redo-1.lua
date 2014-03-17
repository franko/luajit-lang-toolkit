for x=1, 10 do ::redo::
	print(x)
	x = x^2 + 1
	if x < 100 then goto redo end
end
