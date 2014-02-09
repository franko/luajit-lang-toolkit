local function foo(self, k)
    return k * self.value
end

local function create_obj(x)
    return { value = x, foo = foo }
end

print(create_obj(3.141592):foo(2))
