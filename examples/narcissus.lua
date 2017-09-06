data { string = "\031\139\008\000" } -- header
data { string = "\000\000\000\000" } -- header
data { string = "\000\003" }         -- header
print { string = "hi there" }
rep { from = -6, len = 6 }
data { string = "\154\172\013\201" } -- checksum
data { string = "\014\000\000\000" } -- size



--data { from = 2, to = 5 }
