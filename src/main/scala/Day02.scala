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

  // loop over test data.  Test data had multiple rows and real data has only 1
  for(pgm <- lines) {

    var i = 0
    //Once you have a working computer, the first step is to restore the
    // gravity assist program (your puzzle input) to the "1202 program alarm" state
    // it had just before the last computer caught fire. To do this, before running the program,
    // replace position 1 with the value 12 and replace position 2 with the value 2.
    // What value is left at position 0 after the program halts?
    pgm(1) = 12
    pgm(2) = 2
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
    println(s"Answer Part One:  ${results(0)}")
    //println(s"\n\n Next Case:")
  }


  bufferedSource.close


}
