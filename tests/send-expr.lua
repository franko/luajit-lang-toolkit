local boo = { value = 7, compute= function(self, i) return self.value*i+1 end }
print(boo:compute(3) + boo:compute(7))

