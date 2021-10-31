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

  println(lines)

  val t1 = System.nanoTime

  val wire1: Vector[String] = lines(0).split(",").toVector
  val wire2: Vector[String] = lines(1).split(",").toVector
  println(s"Wire 1: $wire1")
  println(s"Wire 2 $wire2")

  // brute force?  make a list of all the points each wire touches and
  // find coordinates the two lists have in common
  val w1pts = List()

  //for (p <== wire1)




  val duration = (System.nanoTime - t1) / 1e9d

  val results = List(0)
  println(s"Answer Part One:  ${results(0)}")
  println(s"Run time (by the clock) of Part Three: $duration sec")



}
