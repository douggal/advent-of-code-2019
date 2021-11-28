val x = 123456

val xs = x.toString.toVector


val lineOfCode = "1002,4,3,4"
val instrSeq = lineOfCode.split(',').toSeq.map(_.toInt)
val opcode = instrSeq.head.toString


val instr = f"${instrSeq.head}%05d"
val opcode = (instr(3).toString + instr(4).toString).toInt

