import scala.collection.mutable.ListBuffer

object Day04 extends App{

  // created 11/14/2021
  // https://adventofcode.com/2019/day/3

  println(s"--- Day 4: Secure Container ---")

  //val puzzleInput = "146810-612564"
  val puzzleInput = "222688-222689" // "146810-612564"  //111123

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


  val candidates2 =  new ListBuffer[Int]()

  // returns true if pair found and are "not part of a larger group of matching digits"
  def countPairs(ps: List[Int]): Boolean = {
    // map of digits to 1 or 0.  1 = 1 or more separate pairs found, 0 = no pairs found
    val m = scala.collection.mutable.Map[Int, Int]()
    // for each digit
    for (i <- 0 to 9) {
      m(i) = 0
        // walk down the list of digits digit at a time comparing pairs
        for (j <- 1 to ps.length-1) {
          println(s"${ps(j-1)} ${ps(j)}")
          if (ps(j-1) == i && ps(j-1) == ps(j)) {
            // if pair found the set map = 1 else more than two in a row or no match set = 0

            if (m(i) >= 1) {
              println(s"Here ${m(i)}")
              m(i) = 0
            } else m(i) = 1
          }  // else do nothing no matched pair found
        }
    }
    // if any value in the Map is gt 0 then we have a winning candidate passwords
    println(m)
    if (m.valuesIterator.max > 0) true else false
  }

  for (p <- candidates) {
    val ps = p.toString.toList.map(_.asDigit)
    if (countPairs(ps)) candidates2 += p
  }

  println(s"Answer Part Two (nbr passwords that meet single pair criteria):  ${candidates2.length}")

}

