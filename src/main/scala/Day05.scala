object Day05 extends App{

  import scala.collection.immutable.HashMap

  // created 11/25/2021
  // https://adventofcode.com/2019/day/5

  // 0: instruction or opcode (1, addition),
  // 1,3: the positions of the two inputs (9 and 10),
  // 3: and the position of the output (3)
  //

  println(s"--- Day 5: Sunny with a Chance of Asteroids ---")

  val filename = "Day02.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)

  // Intcode 2000 an implementation of an Intcode computer
  def Intcode2000(pgm: Array[Int]): Array[Int] = {
    // blockSize = width of opcode + its  operand(s) + storage address
    val blockSize = HashMap((0->1),(1->4),(2->4),(99->1))  //.withDefaultValue("Not found")

    var ip = 0
    var exit: Boolean = false
    while (ip < pgm.length & !exit) {
      val instr = pgm.slice(ip,ip+blockSize(pgm(ip))).toList
      //println(s"$instr => block size ${blockSize(pgm(ip))}")
      instr.head match {
        case 1 =>
          // println("Add")
          pgm(instr(3)) = pgm(instr(1)) + pgm(instr(2))
        case 2 =>
          // println("Multiply")
          pgm(instr(3)) = pgm(instr(1)) * pgm(instr(2))
        case 3 => ???
        case 4 => ???
        case 99 =>
          // println("Exit")
          exit = true
        case _ =>
          println("Error")
          exit = true
      }
      ip += blockSize(instr.head)
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

    //Once you have a working computer, the first step is to restore the
    // gravity assist program (your puzzle input) to the "1202 program alarm" state
    // it had just before the last computer caught fire. To do this, before running the program,
    // replace position 1 with the value 12 and replace position 2 with the value 2.
    // What value is left at position 0 after the program halts?
    pgm(1) = 12
    pgm(2) = 2
    val results = Intcode2000(pgm).toList
    //println(s"Result\n:${results}")
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
