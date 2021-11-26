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

  val systemID = 1 // the ship's air conditioner unit

  // Intcode 2000 an implementation of an Intcode computer
  def Intcode2000(pgm: Array[Int]): Array[Int] = {
    // blockSize = width of instruction (opcode + its  operand(s) + storage address)
    val blockSize = HashMap((0->1),(1->4),(2->4),(3->2),(4->2),(99->1))  //.withDefaultValue("Not found")

    var ip = 0
    var exit: Boolean = false
    while (ip < pgm.length & !exit) {
      val lineOfCode = pgm.slice(ip,ip+blockSize(pgm(ip))).toList
      println(s"$lineOfCode => block size ${blockSize(pgm(ip))}")
      /*
      ABCDE
       1002

        DE - two-digit opcode,      02 == opcode 2
         C - mode of 1st parameter,  0 == position mode
         B - mode of 2nd parameter,  1 == immediate mode
         A - mode of 3rd parameter,  0 == position mode,
                                          omitted due to being a leading zero
      */
      // represent instruction as 5 char string with leading spaces padded with '0's
      val instr = f"${lineOfCode.head}%05d"
      val opcode = (instr(3).asDigit.toString + instr(4).asDigit.toString).toInt
      // modes in order, i.e., modes(0) = mode of 1st param, etc
      // mode 0 = position mode
      // mode 1 = immediate mode
      val modes = Vector[Int](instr(2).asDigit,instr(1).asDigit,instr(0).asDigit)

      opcode match {
        case 1 =>
          // println("Add")
          pgm(instr(3)) = pgm(instr(1)) + pgm(instr(2))
        case 2 =>
          // println("Multiply")
          pgm(instr(3)) = pgm(instr(1)) * pgm(instr(2))
        case 3 =>
          //println("Input")
          val paramDirection = 0
        case 4 =>
          //println(Output)
          val paramDirection = 0
        case 99 =>
          // println("Exit")
          exit = true
        case _ =>
          println(s"Error opcode ip ${ip}, $opcode, modes $modes")
          exit = true
      }
      ip += blockSize(opcode)
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
