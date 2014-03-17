for z=1,10 do
	for y=1,10 do
		for x=1,10 do
  			if x^2 + y^2 == z^2 then
    			print('found a Pythagorean triple:', x, y, z)
    			goto done
  			end
		end
	end
end
::done::
