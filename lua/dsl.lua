debug.print = print

function add_src_info(args)
  local d = debug.getinfo(3, "Sl")
  args._file = d.short_src
  args._line = d.currentline
end

program = { }
function rep(args)
  args.type = "rep"
  if args.from and args.to and not args.len then
    args.len = args.to - args.from
  end
  add_src_info(args)
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
  add_src_info(args)
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
  add_src_info(args)
  table.insert(program, args)
end

function _(str)
  args = { type = "label", name = str }
  add_src_info(args)
  table.insert(program, args)
end

l = {}
label_mt = { __index = function() return 0 end }
label_err_mt = { __index = function(self, key) error("Unknown label: " .. key, 2) end }
setmetatable(l, label_mt)
