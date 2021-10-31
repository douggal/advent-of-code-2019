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

  def manhattan(q: Tuple2[Int,Int]): Int = {
    // Manhattan distance.  Starting point (p1,p2) is always (0,0)
    q._1.abs + q._2.abs
  }

  println(s"--- Day 3: Crossed Wires ---")

  val filename = s"Day03.txt"
  val lines = readFile(filename)

  //println(lines)

  val t1 = System.nanoTime

  val wire1: Vector[String] = lines(0).split(",").map(_.trim).toVector
  val wire2: Vector[String] = lines(1).split(",").map(_.trim).toVector
  //println(s"Wire 1: $wire1")
  //println(s"Wire 2 $wire2")

  // brute force?  make a list of all the points each wire touches and
  // find coordinates the two lists have in common
  var wire1path = List[Tuple2[Int,Int]]()
  var wire2path = List[Tuple2[Int,Int]]()

  wire1path = (0,0)::Nil
  wire2path = (0,0)::Nil

  for (p <- wire1) {
    //print(s"${p.head},${p.tail} : ")
    p.head match {
      case 'R' | 'L' =>
        val inc = if (p.head == 'R') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          val currLoc = (wire1path.head._1 + inc, wire1path.head._2)
          wire1path = currLoc :: wire1path
        }
      case 'U' | 'D' => {
        val inc = if (p.head == 'U') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          val currLoc = (wire1path.head._1, wire1path.head._2 + inc)
          wire1path = currLoc :: wire1path
        }
      }
      case _ => var i = 0
    }
  }

  for (p <- wire2) {
    //print(s"${p.head},${p.tail} : ")
    p.head match {
      case 'R' | 'L' =>
        val inc = if (p.head == 'R') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          val currLoc = (wire2path.head._1 + inc, wire2path.head._2)
          wire2path = currLoc :: wire2path
        }
      case 'U' | 'D' => {
        val inc = if (p.head == 'U') 1 else -1
        for (x <- 1 to p.tail.toInt) {
          val currLoc = (wire2path.head._1, wire2path.head._2 + inc)
          wire2path = currLoc :: wire2path
        }
      }
      case _ => println("Error")
    }
  }

  // convert to Set, chop off the (0,0) starting point
  val circuit1 = wire1path.dropRight(1).toSet
  val circuit2 = wire2path.dropRight(1).toSet

  val res = circuit1.intersect(circuit2).map(p => manhattan(p)).min

  // find points where the wires cross

  val duration = (System.nanoTime - t1) / 1e9d

  println(s"Answer Part One:  ${res}")
  println(s"Run time (by the clock) of Part Three: $duration sec")

}
