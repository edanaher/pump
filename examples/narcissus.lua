fextra_size = l.fextra_end - l.secondcopy

function gzip_header(fextra_size, filename)
  local flags = 0
  --if fextra_size > 0 then flags = flags + 4 end
  flags = flags + 4
  if filename then flags = flags + 8 end
  data { string = "\031\139\008" } -- header: id1, id2, compression method
  data { int = flags, size = 1 } -- FLGS
  data { string = "\000\000\000\000" } -- mtime
  data { string = "\000\003" }         -- XFL, OS

  data { int = fextra_size, size = 2} -- fextra length
end

function gzip_footer()
  data { string = "\000\000\000\000" } -- checksum
  data { int = l._end, size = 4} -- size
end

gzip_header(fextra_size, true)
_"secondcopy"
rep { from = 0, to = l.endfirstcopy, at = l.endprint }
rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.secondrep }
gzip_footer()
_"endsecondcopy"
zero { ranges = {{0, l._end}} }
_"fextra_end"
data { string = "narcissus.gz\000" }

_"_start"
print { len = l.endprint - l.endfirstcopy }
_"endfirstcopy"
  data { from = 0, to = l.endfirstcopy }
_"endprint"
rep { from = 0, to = l.endfirstcopy }
_"secondrep"
rep { from = l.secondcopy, to = l.endsecondcopy, final = true }

gzip_footer()
_"_end"
