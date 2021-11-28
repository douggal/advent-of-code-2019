import scala.collection.mutable.ListBuffer

object Day05 extends App{

  import scala.collection.immutable.HashMap

  // created 11/25/2021
  // https://adventofcode.com/2019/day/5

  // 0: instruction or opcode (1, addition),
  // 1,3: the positions of the two inputs (9 and 10),
  // 3: and the position of the output (3)
  //

  println(s"--- Day 5: Sunny with a Chance of Asteroids ---")

  val filename = "Day05.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val lines = bufferedSource
    .getLines
    .map { line =>
      line.split(',').map(_.toInt)
    }

  val systemID = 1 // the ship's air conditioner unit

  // Intcode 2000: an implementation of an Intcode computer
  def Intcode2000(pgm: Array[Int]): Array[Int] = {
    // blockSize = width of instruction (opcode + its  operand(s) + storage address)

    val blockSize = HashMap(0->1,1->4,2->4,3->2,4->2,99->1)  //.withDefaultValue("Not found")
    val stack = ListBuffer[List[Int]]()
    var ip = 0
    var exit: Boolean = false
    while (ip < pgm.length & !exit) {
      //print(s"ip $ip : next7 = ${pgm.slice(ip,ip+7).toList}:  ")

      // represent instruction as 5 char string with leading spaces padded with '0's
      val instr = f"${pgm(ip)}%05d"
      val opcode = (instr(3).asDigit.toString + instr(4).asDigit.toString).toInt

      val lineOfCode = pgm.slice(ip,ip+blockSize(opcode)).toList
      stack.prepend(lineOfCode)

      //println(s" ip $ip : line of code = $lineOfCode : block size ${blockSize(opcode)} ")
      /*
      get the parameter modes, defined as follows:
      ABCDE
       1002

        DE - two-digit opcode,      02 == opcode 2
         C - mode of 1st parameter,  0 == position mode
         B - mode of 2nd parameter,  1 == immediate mode
         A - mode of 3rd parameter,  0 == position mode,
                                          omitted due to being a leading zero
      */
      // Vector of modes parallel to parameter order, i.e., modes(0) = mode of 1st param, etc
      // mode 0 = position mode parameter is interpreted as a address
      // mode 1 = immediate mode parameter is interpreted as a value
      val modes = Vector[Int](instr(2).asDigit,instr(1).asDigit,instr(0).asDigit)

      opcode match {
        case 1 =>
          // println("Add") takes 2 parameters or operands
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          pgm(lineOfCode(3)) = op1 + op2
        case 2 =>
          // println("Multiply") takes 2 operands
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          pgm(lineOfCode(3)) = op1 * op2
        case 3 =>
          //println("Input") takes one parameter and saves it this address
          // get the input from user
          // cannot be in immediate mode
          print("Enter input: ")
          val a = scala.io.StdIn.readInt()
          pgm(lineOfCode(1)) = a
        case 4 =>
          //println(Output) take one parameter an address and outputs value at the address
          // cannot be in immediate mode
          if (modes(0)==0) println(s"ip $ip: ${pgm(lineOfCode(1))}") else lineOfCode(1)
//          if (pgm(lineOfCode(1)) != 0) {
//            println("Not 0 output:  dumping stack")
//            for (item <- stack) println(item)
//          }
        case 99 =>
          println("Halt")
          exit = true
        case _ =>
          println(s"Error ip $ip, lineOfCode $lineOfCode, opcode $opcode, modes $modes")
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


  // loop over test data.
  // Test data file had multiple rows but real data has only 1 row
  for(pgm <- lines) {

    /*
    The TEST diagnostic program will start by requesting from the user the ID of the
    system to test by running an input instruction - provide it 1, the ID for the
    ship's air conditioner unit.

    It will then perform a series of diagnostic tests confirming that various
    parts of the Intcode computer, like parameter modes, function correctly.
    For each test, it will run an output instruction indicating how far the result
    of the test was from the expected value, where 0 means the test was successful.
    Non-zero outputs mean that a function is not working correctly; check the
    instructions that were run before the output instruction to see which one failed.
    */
    val results = Intcode2000(pgm).toList

    //println(s"Result\n:$results")
    println(s"Answer Part One:  the output immediately before the 'Halt'")
    //println(s"\n\n Next Case:")

  }

  bufferedSource.close
}
