data { string = "\031\139\008\000" } -- header
data { string = "\000\000\000\000" } -- header
data { string = "\000\003" }         -- header
print { string = "hi there" }
rep { from = 2, len = 6 }
rep { from = 0, len = 3, final = true }
data { string = "\027\001\083\186" } -- checksum
data { string = "\017\000\000\000" } -- size



--data { from = 2, to = 5 }
