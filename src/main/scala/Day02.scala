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
                line.split(',').map(_.toInt).toList
              }
  for(line <- lines) {
    val instr = line.slice(0,blockSize): List[Int]
    println(s"$instr")
  }


  bufferedSource.close


}
