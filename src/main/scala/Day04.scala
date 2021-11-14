import scala.collection.mutable.ListBuffer

object Day04 extends App{

  // created 11/14/2021
  // https://adventofcode.com/2019/day/3

  println(s"--- Day 4: Secure Container ---")

  //val puzzleInput = "146810-612564"
  val puzzleInput = "146810-612564"  //111123

  // List[Int] list of all the numbers in puzzle input range with non-decreasing
  // digits.  Each entry is a list of digits in the number

  val rangeFrom: Int = puzzleInput.split('-')(0).toInt
  val rangeTo: Int = puzzleInput.split('-')(1).toInt

  //check list of digits if any pair is out of numeric order
  //compare list head to next item returning false if not in order
  //otherwise check list tail
  //if the end of the list is reached return true
  def checkOrdering(xs: List[Int]):Boolean = {
    if (xs.isEmpty | xs.length == 1) true
    else {
      val compare = xs.head <= xs.tail.head
      if (compare) checkOrdering(xs.tail)
      else false
    }
  }

  // check if list has pairs of matching digits
  // ?? does only 2 count or does 3 or more count as matching pair too
  def checkPair(a: Int, b: Int): Boolean = (a == b)

  def checkMatchedPair(xs: List[Int]): Boolean = {
    if (xs.isEmpty) false
    else if (xs.length == 1) false
    else {
      val compare = checkPair(xs.head,xs.tail.head)
      if (compare) {
        true
      }
      else checkMatchedPair(xs.tail)
    }
  }

  val candidates =  new ListBuffer[Int]()

  // for each integer in the range
  for (x <- (rangeFrom to rangeTo)) {
    // turn integer into a list of digits
    val xs = x.toString.toList.map(_.toInt)
    // are the digits in non-decreasing order
    val inOrder = checkOrdering(xs)
    if (inOrder) {
      //does this list have at lease one matched pair of digits
      if (checkMatchedPair(xs)) {
        candidates += x
      }
    }
  }
  println(candidates)

  println(s"Answer Part One (nbr passwords that meet criteria):  ${candidates.length}")

}

