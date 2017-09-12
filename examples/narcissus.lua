data { string = "\031\139\008\004" } -- header
data { string = "\000\000\000\000" } -- header
data { string = "\000\003" }         -- header

data { string = "\024\000" } -- fextra length
_"secondcopy"
rep { from = 0, to = l.endfirstcopy, at = l.endprint } -- WRONG! This should be at = l.endprint
rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.secondrep } -- WRONG! This should be after second rep

data { string = "EVAN" } -- checksum
data { string = "\106\000\000\000" } -- size

_"endsecondcopy"
_"_start"
print { len = l.endprint - l.endfirstcopy }
_"endfirstcopy"
  data { string = "\031\139\008\004" } -- header
  data { string = "\000\000\000\000" } -- header
  data { string = "\000\003" }         -- header
  data { string = "\024\000" } -- fextra length
  rep { from = 0, to = l.endfirstcopy, at = l.endprint }
  rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.secondrep }

  data { string = "EVAN" } -- checksum
  data { string = "\106\000\000\000" } -- size

  print { len = l.endprint - l.endfirstcopy}
_"endprint"
rep { from = 0, to = l.endfirstcopy }
_"secondrep"
rep { from = l.secondcopy, to = l.endsecondcopy, final = true }

data { string = "EVAN" } -- checksum
data { string = "\106\000\000\000" } -- size
