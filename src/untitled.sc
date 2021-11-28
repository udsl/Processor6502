
val x = "12345,"
x.substring(0, x.length-1)
x.substring(x.length-1, x.length)
x.dropRight(1)
x.takeRight(1)
val reg = "^\\$[0-9a-fA-F]+$".r
reg.findFirstMatchIn("$ae")
reg.findFirstMatchIn("$1Af")
reg.findFirstMatchIn("$ag")