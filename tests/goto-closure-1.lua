local ls = { }

for z=1,10 do
for y=1,10 do
for x=1,10 do
  if x^2 + y^2 == z^2 then
    ls[#ls+1] = function() return x, y, z end
    goto zcontinue
  end
end end ::zcontinue:: end

for i = 1, #ls do
    print('found a Pythagorean triple', ls[i]())
end
