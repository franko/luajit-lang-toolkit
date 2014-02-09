local function foo(v, i)
	v.f[i+1], v.x, v.s[i] = i*i, i+5, 7*i
end

local v = { }
v.f, v.s, ciao = v, v, 'ciccio'
foo(v, 1)
print(#v, v[1], v[2])
