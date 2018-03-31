debug.print = print

function add_src_info(args)
  local d = debug.getinfo(3, "Sl")
  args._file = d.short_src
  args._line = d.currentline
end

program = { }
errors = { }
function rep(args)
  args.type = "rep"
  if args.from and args.to and not args.len then
    args.len = args.to - args.from
  end
  if not args.from or not args.len then
    err = { err = "Rep command must have from and len or to" }
    add_src_info(err)
    table.insert(errors, err)
    return
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
  else
    err = { err = "Data command must have string, int, or from field" }
    add_src_info(err)
    table.insert(errors, err)
    return
  end
  add_src_info(args)
  table.insert(program, args)
end

function print(args)
  if args.string or args.len then
    args.type = "print"
    --[[if args.len and (args.len < 0 or args.len > 65535) then
      err = { err = "Print len must be between 0 and 65535" }
      add_src_info(err)
      table.insert(errors, err)
      return
    end]]
    if args.string and #args.string > 65535 then
      err = { err = "Print string can't be longer than 65535" }
      add_src_info(err)
      table.insert(errors, err)
      return
    end
  elseif args.from then
    args.type = "copy"
    if args.from and args.to and not args.len then
      args.len = args.to - args.from
    end
  else
    err = { err = "Print command must have string, len, or from field" }
    add_src_info(err)
    table.insert(errors, err)
    return
  end
  add_src_info(args)
  table.insert(program, args)
end

function zero(args)
  args.type = "zero"
  add_src_info(args)
  table.insert(program, args)
end

function _(str)
  args = { type = "label", name = str }
  add_src_info(args)
  table.insert(program, args)
end

function mkLabel(val)
  res = {}
  if(type(val) == "table") then
    return val
  elseif(type(val) == "string") then
    res = { type = "addr", case = "label", value = val }
  elseif(type(val) == "number") then
    res = { type = "addr", case = "constant", value = val }
  end

  setmetatable(res, label_obj_metatable)
  return res
end

label_obj_metatable = {
  __add = function(self, other) return { type = "addr", case = "plus", [1] = self, [2] = other } end,
  __sub = function(self, other) return { type = "addr", case = "minus", [1] = self, [2] = other } end,
}


l = {}
label_mt = { __index = function(self, key) return mkLabel(key) end }
setmetatable(l, label_mt)
