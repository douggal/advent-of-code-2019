import scala.io.Source

object Day03 extends App {

  // created 10/16/2021
  // https://adventofcode.com/2019/day/3

  // from: https://alvinalexander.com/source-code/scala-function-read-text-file-into-array-list/
  def readFile(filename: String): Array[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toArray
    bufferedSource.close
    lines
  }

  println(s"--- Day 3: Crossed Wires ---")

  val filename = s"Day03.txt"
  val lines = readFile(filename)

  //println(lines)

  val t1 = System.nanoTime

  val wire1: Vector[String] = lines(0).split(",").map(_.trim).toVector
  val wire2: Vector[String] = lines(1).split(",").map(_.trim).toVector
  println(s"Wire 1: $wire1")
  println(s"Wire 2 $wire2")

  // brute force?  make a list of all the points each wire touches and
  // find coordinates the two lists have in common
  var circuit = List[Tuple2[Int,Int]]()

  var currLoc = (0,0)

  for (p <- wire1) {
    print(s"${p.head},${p.tail} : ")
    p.head match {
      case 'R' | 'L' =>
        val inc = if (p.head == 'R') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          currLoc = (currLoc._1 + inc, currLoc._2)
          circuit = currLoc :: circuit
        }
      case 'U' | 'D' => {
        val inc = if (p.head == 'U') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          currLoc = (currLoc._1, currLoc._2 + inc)
          circuit = currLoc :: circuit
        }
      }
      case _ => var i = 0
    }
  }
  println()
  println(circuit)



  val duration = (System.nanoTime - t1) / 1e9d

  val results = List(0)
  println(s"Answer Part One:  ${results.head}")
  println(s"Run time (by the clock) of Part Three: $duration sec")



}
