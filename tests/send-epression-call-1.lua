local function foo(self, k)
    return k * self.value
end

local obj = { value = 3.141592, foo = foo }
print(obj:foo(2))
