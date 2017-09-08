data { string = "\031\139\008\000" } -- header
data { string = "\000\000\000\000" } -- header
data { string = "\000\003" }         -- header

--data { string = "\004\000" } -- fextra length
--data { string = "Evan" }
_"_start"
print { len = 21 }
data { string = "1234567890hello there" }
--print { string = "1234567890hello there" }
rep { from = -6, len = 6 }
rep { from = l._start, len = 5, final = true }
data { string = "\135\223\170\025" } -- checksum
data { string = "\032\000\000\000" } -- size

--data { from = 2, to = 5 }
