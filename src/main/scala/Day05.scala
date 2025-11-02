import scala.collection.mutable.ListBuffer

object Day05 extends App{

  import scala.collection.immutable.HashMap

  // created 11/25/2021
  // https://adventofcode.com/2019/day/5


  println(s"--- Day 5 ---")

  val filename = "Day05test.txt"
  val bufferedSource = scala.io.Source.fromFile(filename)
  val lines = bufferedSource
    .getLines
    .map { line =>
      line.split(',').map(_.toInt)
    }

  val systemID = 1 // the ship's air conditioner unit

  def dumpStack(stack: ListBuffer[List[Int]]):Unit = {
    println("Dumping stack")
    for (item <- stack) println(item)
  }

  def dumpPgm(program: Array[Int]):Unit = {
    println("Dumping program")
    for (item <- program) print(item)
  }

   def Intcode2000(pgm: Array[Int]): Array[Int] = {
    // blockSize = width of instruction (opcode + its  operand(s) + storage address)

    val blockSize = HashMap(0->1,1->4,2->4,3->2,4->2,5->3,6->3,7->4,8->4,99->1)  //.withDefaultValue("Not found")
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

      val modes = Vector[Int](instr(2).asDigit,instr(1).asDigit,instr(0).asDigit)

      opcode match {
        case 1 =>
          // println("Add") takes 2 parameters or operands
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          pgm(lineOfCode(3)) = op1 + op2
          ip += blockSize(opcode)
        case 2 =>
          // println("Multiply") takes 2 operands
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          pgm(lineOfCode(3)) = op1 * op2
          ip += blockSize(opcode)
        case 3 =>
          //println("Input") takes one parameter and saves it this address
          // get the input from user
          // cannot be in immediate mode
          print("Enter input: ")
          val a = scala.io.StdIn.readInt()
          pgm(lineOfCode(1)) = a
          ip += blockSize(opcode)
        case 4 =>
          //println(Output) take one parameter an address and outputs value at the address
          // cannot be in immediate mode
          if (modes(0)==0) println(s"ip $ip: ${pgm(lineOfCode(1))}") else lineOfCode(1)
//          if (pgm(lineOfCode(1)) != 0) {
//            println("Not 0 output:  dumping stack")
//            for (item <- stack) println(item)
//          }
          ip += blockSize(opcode)
        case 5 =>
          // jump-if-true
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          if (op1 != 0) {
            val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
            ip = op2
          } else ip += blockSize(opcode)
        case 6 =>
          //jump-if-false
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          if (op1 == 0) {
            val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
            ip = op2
          } else  ip += blockSize(opcode)
        case 7 =>
          // less than
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          val op3 = if (modes(2)==0) pgm(lineOfCode(3)) else lineOfCode(3)
          if (op1 < op2) {
            pgm(op3) = 1
          } else {
            pgm(op3) = 0
          }
          ip += blockSize(opcode)
        case 8 =>
          // equal
          val op1 = if (modes(0)==0) pgm(lineOfCode(1)) else lineOfCode(1)
          val op2 = if (modes(1)==0) pgm(lineOfCode(2)) else lineOfCode(2)
          val op3 = if (modes(2)==0) pgm(lineOfCode(3)) else lineOfCode(3)
          if (op1 == op2) {
            pgm(op3) = 1
          } else {
            pgm(op3) = 0
          }
          ip += blockSize(opcode)
        case 99 =>
          println("Halt")
          exit = true
        case _ =>
          println(s"Error ip $ip, lineOfCode $lineOfCode, opcode $opcode, modes $modes")
          exit = true
      }
    }
    dumpStack(stack)  //debug
    dumpPgm(pgm)
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


    val results = Intcode2000(pgm).toList

    //println(s"Result\n:$results")
    println(s"Answer Part One")
    //println(s"\n\n Next Case:")

  }

  bufferedSource.close
}
