program = { }
function rep(args)
  args.type = "rep"
  table.insert(program, args)
end

function data(args)
  args.type = "data"
  table.insert(program, args)
end

print("DSL loaded!")
loaded = 99
