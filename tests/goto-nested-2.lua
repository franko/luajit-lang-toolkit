for z=1,10 do
	for y=1,10 do
		for x=1,10 do
	  		if x^2 + y^2 == z^2 then
	    		print('found a Pythagorean triple:', x, y, z)
	    		print('now trying next z...')
	    		goto zcontinue
	  		end
		end
	end
	::zcontinue::
end
