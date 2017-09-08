data { string = "\031\139\008\004" } -- header
data { string = "\000\000\000\000" } -- header
data { string = "\000\003" }         -- header

data { string = "\024\000" } -- fextra length
_"secondcopy"
rep { from = 0, to = l.endfirstcopy, at = l.endfirstcopy } -- WRONG! This should be at = l.endprint
rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.endprint } -- WRONG! This should be after second rep

data { string = "\078\139\085\248" } -- checksum
data { string = "\106\000\000\000" } -- size

_"endsecondcopy"
_"_start"
print { len = l.endprint - l.endfirstcopy }
_"endfirstcopy"
  data { string = "\031\139\008\004" } -- header
  data { string = "\000\000\000\000" } -- header
  data { string = "\000\003" }         -- header
  data { string = "\024\000" } -- fextra length
  rep { from = 0, to = l.endfirstcopy, at = l.endfirstcopy, isdata = true }
  rep { from = l.secondcopy, to = l.endsecondcopy, final = true, at = l.endprint, isdata = true }

  data { string = "\078\139\085\248" } -- checksum
  data { string = "\106\000\000\000" } -- size

  print { len = l.endprint - l.endfirstcopy, isdata = true }
_"endprint"
rep { from = 0, to = l.endfirstcopy }
rep { from = l.secondcopy, to = l.endsecondcopy, final = true }


data { string = "\078\139\085\248" } -- checksum
data { string = "\106\000\000\000" } -- size
