fextra_size = l.fextra_end - l.fextra

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
_"filename"
_"fextra"
zero { ranges = {{l.filename, l.shared_fextra}} }
data { string = "tweedledee.gz\000" }
_"shared_fextra"
zero { ranges = {{0, l._end}} }
_"footercopy"
data { from = l.reps, to = l.footer }
gzip_footer()
_"endfooter"
_"otherfilename"
zero { ranges = {{l.otherfilename, l.copy_end}} }
_"fextra_end"
data { string = "tweedledum.gz\000" }
_"copy_end"

_"_start"
print { len = l.endprint - l.endfirstcopy }
_"endfirstcopy"
  data { from = 0, to = l.filename }
  data { from = l.otherfilename, to = l.copy_end }
  data { from = l.shared_fextra, to = l.otherfilename}
  data { from = l.filename, to = l.shared_fextra }
  data { from = l.copy_end, to = l.endfirstcopy }
_"endprint"

_"reps"
rep { from = 0, to = l.filename }
rep { from = l.otherfilename, to = l.copy_end }
rep { from = l.shared_fextra, to = l.otherfilename}
rep { from = l.filename, to = l.shared_fextra }
rep { from = l.copy_end, to = l.endfirstcopy }

rep { from = l.footercopy, to = l.endfooter, final = true }

_"footer"

gzip_footer()
_"_end"
