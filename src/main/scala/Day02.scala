import scala.io.Source

object Day02 extends App {

  // created 9/14/2021
  // https://adventofcode.com/2019/day/2


  // 0: opcode (1, addition),
  // 1,3: the positions of the two inputs (9 and 10),
  // 3: and the position of the output (3)

  println(s"--- Day 2 ---")

  val filename = s"Day02.txt"
  val bufferedSource = Source.fromFile(filename)

  def Intcode2000(pgm: Array[Int]): Array[Int] = {
    val blockSize = 4
    var ip = 0
    var exit: Boolean = false
    while (ip < pgm.length & !exit) {
      val instr = pgm.slice(ip,ip+blockSize).toList
      //print(s"$instr ")
      instr.head match {
        case 1 =>
          // println("Add")
          pgm(instr(3)) = pgm(instr(1)) + pgm(instr(2))
        case 2 =>
          // println("Multiply")
          pgm(instr(3)) = pgm(instr(1)) * pgm(instr(2))
        case 99 =>
          // println("Exit")
          exit = true
        case _ =>
          println("Error")
          exit = true
      }
      ip += blockSize
    }
    pgm
  }

  def test(pgm: Array[Int], noun:Int, verb:Int): Int = {
    val pgmTmp = pgm.clone()
    pgmTmp(1) = noun
    pgmTmp(2) = verb
    Intcode2000(pgmTmp)(0)
  }

  val lines = bufferedSource
              .getLines
              .map { line =>
                line.split(',').map(_.toInt).toArray
              }

  // loop over test data.
  // Test data file had multiple rows but real data has only 1 row
  for(pgm <- lines) {

    val clone = pgm.clone

    pgm(1) = 12
    pgm(2) = 2
    val results = Intcode2000(pgm).toList
    println(s"Result\n:${results}")
    println(s"Answer Part One:  ${results(0)}")
    //println(s"\n\n Next Case:")

    val t1 = System.nanoTime
    for (
      noun <- 0 to 99;
      verb <- 0 to 99
    ) {
        if (test(clone.clone(), noun, verb) == 19690720) println(s"Found i = $noun, j = $verb.  Answer Part Two: ${100 * noun + verb}")
        // break out - how?
      }
    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Duration of Part Two: $duration sec")
  }


  bufferedSource.close


}
