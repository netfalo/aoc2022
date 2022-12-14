import scala.annotation.tailrec

object Day14 extends Problem[Int, Int] {
  private val source: Point = Point(500, 0)
  private val sandMark = 'o'

  override def solve(puzzle: String): Solution[Int, Int] = {
    val (offset, max, lines) = parseLines(puzzle)
    val map = generateMap(max, lines)
    val sourceWithOffset = source - offset;

    Solution(solveFirstPart(sourceWithOffset, map), solveSecondPart(sourceWithOffset, map))
  }

  def solveFirstPart(source: Point, map: Vector[String]): Int = {
    dropUntilItPoursDown(source, map)
      .map(_.count(_ == sandMark))
      .sum
  }

  def solveSecondPart(source: Point, map: Vector[String]): Int = {
    val mapWithInfiniteFloor = map
      .appended(Vector.fill(map(0).length)('.').mkString)
      .appended(Vector.fill(map(0).length)('#').mkString)

    val result = dropUntilItPoursDownWithInfiniteFloor(source, mapWithInfiniteFloor)
    println(result.mkString("\n"))

    result.map(_.count(_ == sandMark))
      .sum
  }

  def parseLines(puzzle: String): (Point, Point, Vector[Line]) = {
    val points = puzzle
      .split("\n")
      .filterNot(_.isBlank)
      .flatMap(parsePath)
      .toVector

    val pointList = points
      .flatMap { case Line(left, right) => Array(left, right) }
      .appended(source)

    val offsetX = pointList
      .minBy(_.x)
    val offsetY = pointList
      .minBy(_.y)

    val maxX = pointList
      .maxBy(_.x)
    val maxY = pointList
      .maxBy(_.y)

    val offset = Point(offsetX.x, offsetY.y)
    val max = Point(maxX.x, maxY.y) - offset

    (offset, max,
      points
        .map { case Line(start, end) => Line(start - offset, end - offset) })
  }

  def parsePath(path: String): Vector[Line] =
    path
      .split(" -> ")
      .map(parsePoint)
      .sliding(2)
      .map { case Array(first, second) => Line(first, second) }
      .toVector

  def parsePoint(point: String): Point = {
    val Array(x, y) = point
      .split(",")
      .map(Integer.parseInt)

    Point(x, y)
  }

  def generateMap(max: Point, lines: Vector[Line]): Vector[String] = {
    val map =
      Vector.fill(max.y + 1)(Vector.fill(max.x + 1)('.').mkString)

    lines
      .flatMap {
        case Line(start, end) =>
          for {
            x <- if (start.x < end.x) start.x to end.x else end.x to start.x
            y <- if (start.y < end.y) start.y to end.y else end.y to start.y
          } yield Point(x, y)
      }
      .foldLeft(map) {
        case (acc, Point(x, y)) =>
          acc
            .updated(y, acc(y).updated(x, '#'))
      }
  }

  def nextMove(sand: Point, map: Vector[String]): Option[Point] = {
    Seq(
      sand + Point(0, 1),
      sand + Point(-1, 1),
      sand + Point(1, 1)
    )
      .filterNot { case Point(x, y) => x >= 0 && x < map(0).length && y >= 0 && y < map.length && map(y)(x) != '.' }
      .headOption
  }

  @tailrec
  def dropOneSandParticle(sand: Point, map: Vector[String]): Option[Vector[String]] = {
    val move = nextMove(sand, map)
    if (move.isEmpty) Some(map.updated(sand.y, map(sand.y).updated(sand.x, sandMark)))
    else if (move.get.x < 0 || move.get.x >= map(0).length || move.get.y < 0 || move.get.y >= map.length) None
    else dropOneSandParticle(move.get, map)
  }

  def nextMoveWithInfiniteFloor(sand: Point, map: Vector[String]): Option[Point] = {
    Seq(
      sand + Point(0, 1),
      sand + Point(-1, 1),
      sand + Point(1, 1)
    )
      .filterNot { case Point(x, y) => y >= map.length - 1 || x >= 0 && x < map.head.length && map(y)(x) != '.' }
      .headOption
  }

  @tailrec
  def dropOneSandParticleWithInfiniteFloor(sand: Point, map: Vector[String], offset: Int): (Vector[String], Int) = {
    val move = nextMoveWithInfiniteFloor(sand, map)
    if (move.isEmpty) (map.updated(sand.y, map(sand.y).updated(sand.x, sandMark)), offset)
    else {
      val (newSand, updatedMap, newOffset) =
        if (move.get.x < 0)
          (Point(move.get.x + 1, move.get.y), map
            .map(_.prepended('.'))
            .updated(map.length - 1, map(map.length - 1).prepended('#')), offset + 1)
        else if (move.get.x >= map(0).length)
          (move.get, map
            .map(_.appended('.'))
            .updated(map.length - 1, map(map.length - 1).appended('#')), offset)
        else (move.get, map, offset)

      dropOneSandParticleWithInfiniteFloor(newSand, updatedMap, newOffset)
    }
  }

  @tailrec
  def dropUntilItPoursDownWithInfiniteFloor(source: Point, map: Vector[String]): Vector[String] = {
    val (updatedMap, offset) = dropOneSandParticleWithInfiniteFloor(source, map, 0)
    if (updatedMap(source.y)(source.x) == sandMark) updatedMap
    else dropUntilItPoursDownWithInfiniteFloor(source + Point(offset, 0), updatedMap)
  }


  @tailrec
  def dropUntilItPoursDown(source: Point, map: Vector[String]): Vector[String] = {
    val dropped = dropOneSandParticle(source, map)
    if (dropped.isEmpty) map
    else dropUntilItPoursDown(source, dropped.get)
  }
}
