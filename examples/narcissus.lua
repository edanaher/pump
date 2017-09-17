fextra_size = l.endsecondcopy - l.secondcopy

function gzip_header(fextra_size)
  data { string = "\031\139\008\004" } -- header
  data { string = "\000\000\000\000" } -- header
  data { string = "\000\003" }         -- header

  data { int = fextra_size, size = 2} -- fextra length
end

function gzip_footer()
  data { string = "EVAN" } -- checksum
  data { int = l._end, size = 4} -- size
end

gzip_header(fextra_size)
_"secondcopy"
rep { from = 0, to = l.endfirstcopy, at = l.endprint } -- WRONG! This should be at = l.endprint
rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.secondrep } -- WRONG! This should be after second rep
gzip_footer()

_"endsecondcopy"
_"_start"
print { len = l.endprint - l.endfirstcopy }
_"endfirstcopy"
  gzip_header(fextra_size)
  rep { from = 0, to = l.endfirstcopy, at = l.endprint }
  rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.secondrep }

  gzip_footer()

  print { len = l.endprint - l.endfirstcopy}
_"endprint"
rep { from = 0, to = l.endfirstcopy }
_"secondrep"
rep { from = l.secondcopy, to = l.endsecondcopy, final = true }

gzip_footer()
_"_end"
