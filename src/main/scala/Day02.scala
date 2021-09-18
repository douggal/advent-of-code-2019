import scala.io.Source

object Day02 extends App {

  // created 9/14/2021
  // https://adventofcode.com/2019/day/2


  // 0: opcode (1, addition),
  // 1,3: the positions of the two inputs (9 and 10),
  // 3: and the position of the output (3)

  println(s"--- Day 2: 1202 Program Alarm ---")

  val filename = s"Day02.txt"
  val bufferedSource = Source.fromFile(filename)
  val blockSize = 4

  val lines = bufferedSource
              .getLines
              .map { line =>
                line.split(',').map(_.toInt).toArray
              }
  for(pgm <- lines) {

    var i = 0
    while (i < pgm.length) {
      val instr = pgm.slice(i,i+blockSize).toList
      print(s"$instr ")
      instr.head match {
        case 1 =>
          println("Add")
          pgm(instr(3)) = pgm(instr(1)) + pgm(instr(2))
        case 2 =>
          println("Multiply")
          pgm(instr(3)) = pgm(instr(1)) * pgm(instr(2))
        case 99 => println("Exit")
        case _ => println("Error")
      }

      i += blockSize
    }
    var results = pgm.toList
    println(s"Result\n:${results}")
    println(s"\n\n Next Case:")
  }


  bufferedSource.close


}
