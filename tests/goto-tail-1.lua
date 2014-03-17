function fact(n, ans)
  ::call::
  if n == 0 then
    return ans
  else
    n, ans = n - 1, ans * n
    goto call
  end
end
print(fact(6, 1))
