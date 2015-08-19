local html = {
  ["<"] = "&lt;",
  [">"] = "&gt;",
  ["&"] = "&amp;"
}

for k, v in pairs(html) do
    print(k, v)
end

