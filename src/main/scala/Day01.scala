import scala.io.Source

object Day01 extends App{

  // created 09/06/2021

  println("--- Day 1: The Tyranny of the Rocket Equation ---")

  /*
  The Elves quickly load you into a spacecraft and prepare to launch.

  At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper. They haven't
  determined the amount of fuel required yet.

  Fuel required to launch a given module is based on its mass. Specifically, to find the fuel
  required for a module, take its mass, divide by three, round down, and subtract 2.
   */

  def calcFuel(m: Int): Int = {
    m / 3 - 2
  }

  // where am I?  where does the input data file need to be?
  println(System.getProperty("user.dir"))

  val filename = s"Day01.txt"
  val bufferedSource = Source.fromFile(filename)
  val massList = bufferedSource.getLines.toList
  val fuelList = massList.map(x => calcFuel(x.toInt))
  val sum = fuelList.reduceLeft(_ + _)
  println(s"Day 01 Part One:  the amount of fuel required is $sum")
  bufferedSource.close



}
