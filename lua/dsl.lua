debug = print

program = { }
function rep(args)
  args.type = "rep"
  if args.from and args.to and not args.len then
    args.len = args.to - args.from
  end
  table.insert(program, args)
end

function data(args)
  if args.string or args.int then
    args.type = "data"
  elseif args.from then
    args.type = "copy"
    if args.from and args.to and not args.len then
      args.len = args.to - args.from
    end
  end
  table.insert(program, args)
end

function print(args)
  if args.string or args.len then
    args.type = "print"
  elseif args.from then
    args.type = "copy"
    if args.from and args.to and not args.len then
      args.len = args.to - args.from
    end
  end
  table.insert(program, args)
end

function _(str)
  table.insert(program, { type = "label", name = str })
end

l = {}
label_mt = { __index = function() return 0 end }
label_err_mt = { __index = function(self, key) error("Unknown label: " .. key, 2) end }
setmetatable(l, label_mt)
