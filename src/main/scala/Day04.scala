import java.awt.event.ContainerListener
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.TailRec

object Day04 extends App{

  // created 11/14/2021
  // https://adventofcode.com/2019/day/3

  println(s"--- Day 4: Secure Container ---")

  //val puzzleInput = "146810-612564"
  //val puzzleInput = "111122-111123" //"146810-612564"  //111123
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
    val xs = x.toString.toList.map(_.asDigit)
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


  // list to be filled with candidate passwords, that is, those that meet the criteria
  var candidates2 =  new ListBuffer[Int]()

  // populates a HashMap with counts (value) each digit (key)
  // input digits are guaranteed to be in non-decreasing order
  def countPairs(ps: List[Int], m: mutable.Map[Int, Int]): Unit = {
    // compare list head to first item in list tail
    // if list is empty we're done or list is down to 1 element we're done
    if (ps.isEmpty || ps.length == 1) ()
    else {
      if (ps.head == ps.tail.head) {
        if (m.contains(ps.head)) m(ps.head) += 1
        else m += (ps.head -> 1)
      }
      countPairs(ps.tail, m)
    }
  }

  // returns true if pair found and are "not part of a larger group of matching digits"
  def testCandidate(ps: List[Int]): Boolean = {
    val m = scala.collection.mutable.Map[Int, Int]()
    countPairs(ps, m)
    // if any digit (key) in HashMap has count of two then this string is possible password
    if (m.values.exists(_ == 2)) true else false
  }

  for (p <- candidates) {
    val ps = p.toString.toList.map(_.asDigit)
    if (testCandidate(ps)) candidates2 += p
  }

  println(candidates2)
  println(s"Answer Part Two (nbr passwords that meet single pair criteria):  ${candidates2.length}")
}

//You guessed 1363 - no go too high
// 481 your answer is too low
// 668