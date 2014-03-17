local function hasfn(t)
	for _, x in ipairs(t) do
		if x % 2 == 0 then
			print 'list has even number'
	    	goto has
	    end
	end
	print 'list lacks even number'
	::has::
end

hasfn( { 1, 3, 7, 9, 4, 11, 13} )
