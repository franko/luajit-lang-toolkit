local function foo(a, flag)
	return a .. (flag and 'boom' or 'pst')
end

print(foo('ciao', true), foo('ciao', false))