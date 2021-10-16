import scala.io.Source

object Day03 extends App {

  // created 10/16/2021
  // https://adventofcode.com/2019/day/3

  println(s"--- Day 3: Crossed Wires ---")

  val filename = s"Day03.txt"
  val bufferedSource = Source.fromFile(filename)

  val t1 = System.nanoTime

  val lines = bufferedSource
    .getLines
    .map { line =>
      line.split(',')
    }

  val results = List(0)

  println(s"Answer Part One:  ${results(0)}")


  val duration = (System.nanoTime - t1) / 1e9d
  println(s"Run time (by the clock) of Part Three: $duration sec")


  bufferedSource.close

}
