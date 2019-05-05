init = function(args)
  local r = {}
  r[1] = wrk.format(nil, "/suggestions?q=mont")
  r[2] = wrk.format(nil, "/suggestions?q=londo")

  from = string.byte("a")
  for i = from, string.byte("z") do
    r[i + 2 - from] = wrk.format(nil, string.format("/suggestions?q=%s", string.sub(i, i)))
  end

  req = table.concat(r)
end

request = function()
  return req
end
