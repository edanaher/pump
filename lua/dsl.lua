program = { }
function rep(args)
  args.type = "rep"
  if args.from and args.to and not args.len then
    args.len = args.to - args.from
  end
  table.insert(program, args)
end

function data(args)
  args.type = "data"
  table.insert(program, args)
end

print("DSL loaded!")
loaded = 99
