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

  // find points where the wires cross - the intersection of the two sets
  // then find Manhattan distance of each point in the set
  // spread it out a bit so we have info for part 2
  // https://alvinalexander.com/scala/union-intersection-difference-scala-sets/
  val xpoints = circuit1.intersect(circuit2)
  val results = xpoints.map(p => manhattan(p))
  println(s"Answer Part One:  ${results.min}")

  // Part 2:  count steps to each intersection along each wire's path
  // find intersection with fewest ssteps to first point of intersection
  val wire1pathrev = wire1path.reverse
  val wire2pathrev = wire2path.reverse
  // I surmise number of steps is equal to index of point in the wire path list
  var steps1 = Vector[Int]()
  var steps2 = Vector[Int]()

  for (xp <- xpoints) {
    // find index of the first occurrence of this point in circuit1
    steps1 = steps1 :+ wire1pathrev.indexOf(xp)

    // find index of the first occurrence of this point in circuit2
    steps2 = steps2 :+ wire2pathrev.indexOf(xp)

  }

  println(s"wire1 ${steps1}")
  println(s"wire2 ${steps2}")

  // sum of corresponding Ints in each vector
  // https://stackoverflow.com/questions/35064883/element-wise-sum-of-arrays-in-scala
  val resultp2 = steps1.zip(steps2).map {case(x,y) => x+y}

  println(s"Answer Part Two:  ${resultp2.min}")

  val duration = (System.nanoTime - t1) / 1e9d
  println(s"Run time (by the clock) of Part Three: $duration sec")




}
