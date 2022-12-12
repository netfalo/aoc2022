import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object Day12 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {
    val (wall, from, to) = parseWall(puzzle)
    Solution(solveFirstPart(from, to, wall).length, solveSecondPart(wall, to).length)
  }

  def solveSecondPart(wall: Vector[String], goal: (Int, Int)): Seq[(Int, Int)] = {
    val startPoints = wall.zipWithIndex.map(x => (0, x._2)).toSet
    val gScore = startPoints.map((_, 0)).toMap
    val fScore = startPoints.map(x => (x, Math.abs(x._1 - goal._1) + Math.abs(x._2 - goal._2))).toMap

    val cameFrom = findShortestRoute(wall, goal, startPoints, Map(), gScore, fScore)
    rebuildPath2(cameFrom, goal, Vector())
  }

  @tailrec
  def rebuildPath2(cameFrom: Map[(Int, Int), (Int, Int)], current: (Int, Int), acc: Vector[(Int, Int)]): Vector[(Int, Int)] = {
    if (current._1 == 0) acc
    else rebuildPath2(cameFrom, cameFrom(current), acc.prepended(current))
  }


  def parseWall(input: String): (Vector[String], (Int, Int), (Int, Int)) = {
    val wall = input.split("\n")
      .filterNot(_.isBlank)
      .toVector
    val startRow = wall.indexWhere(_.contains("S"))
    val startCol = wall(startRow).indexOf("S")

    val endRow = wall.indexWhere(_.contains("E"))
    val endCol = wall(endRow).indexOf("E")

    val updatedStart = wall
      .updated(startRow, wall(startRow).updated(startCol, 'a'))
    val updatedEnd = updatedStart
      .updated(endRow, updatedStart(endRow).updated(endCol, 'z'))

    (updatedEnd, (startCol, startRow), (endCol, endRow))
  }

  @tailrec
  def findShortestRoute(wall: Vector[String], to: (Int, Int), nodesToVisit: Set[(Int, Int)], cameFrom: Map[(Int, Int), (Int, Int)], gScore: Map[(Int, Int), Int], fScore: Map[(Int, Int), Int]): Map[(Int, Int), (Int, Int)] = {
    if (nodesToVisit.isEmpty) throw new RuntimeException("We shouldn't have ended up here")
    val current = nodesToVisit.minBy(fScore(_))
    if (current == to)
      return cameFrom

    val newNodesToVisit = nodesToVisit - current

    val neighbours = Vector((0, -1), (-1, 0), (1, 0), (0, 1))
      .map(n => (n._1 + current._1, n._2 + current._2))
      .filterNot(n => n._1 < 0 || n._2 < 0 || n._1 >= wall.head.length || n._2 >= wall.length)
      .filter(x => wall(x._2)(x._1) - wall(current._2)(current._1) <= 1)
      .filter(x => !gScore.contains(x) || gScore(current) + 1 < gScore(x))

    val updatedGScore = gScore ++ neighbours.map((_, gScore(current) + 1)).toMap
    val updatedFScore = fScore ++ neighbours.map(x => (x, gScore(current) + 1 + Math.abs(x._2 - to._2) + Math.abs(x._1 - to._1))).toMap
    val updatedNodesToVisit = newNodesToVisit ++ neighbours

    val updatedCameFrom = cameFrom ++ neighbours.map(x => (x, current)).toMap
    findShortestRoute(wall, to, updatedNodesToVisit, updatedCameFrom, updatedGScore, updatedFScore)
  }

  @tailrec
  def rebuildPath(from: (Int, Int), cameFrom: Map[(Int, Int), (Int, Int)], current: (Int, Int), acc: Vector[(Int, Int)]): Vector[(Int, Int)] = {
    if (current == from) acc
    else rebuildPath(from, cameFrom, cameFrom(current), acc.prepended(current))
  }

  def solveFirstPart(from: (Int, Int), to: (Int, Int), wall: Vector[String]): Seq[(Int, Int)] = {
    val cameFrom = findShortestRoute(wall, to, Set(from), Map(), Map(from -> 0), Map(from -> (Math.abs(from._1 - to._1) + Math.abs(from._2 - to._2))))
    rebuildPath(from, cameFrom, to, Vector())
  }

}
