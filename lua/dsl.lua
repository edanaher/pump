program = { }
function rep(args)
  args.type = "rep"
  if args.from and args.to and not args.len then
    args.len = args.to - args.from
  end
  table.insert(program, args)
end

function data(args)
  if args.string then
    args.type = "data"
  elseif args.from then
    args.type = "copy"
    if args.from and args.to and not args.len then
      args.len = args.to - args.from
    end
  end
  table.insert(program, args)
end

print("DSL loaded!")
loaded = 99
