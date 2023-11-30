object Day15 {
  case class Sensor(position: Point, closestBeacon: Point, manhattanDistance: Int) {
    def lineCutAtWithoutBeacon(y: Int): Option[Line] = {
      val lateralDistance = manhattanDistance - Math.abs(position.y - y)

      if (lateralDistance <= 0) None
      else {
        val line =
          if (closestBeacon.y == y && closestBeacon.x == position.x - lateralDistance)
            Line(Point(position.x - lateralDistance + 1, y), Point(position.x + lateralDistance, y))
          else if (closestBeacon.y == y && closestBeacon.x == position.x + lateralDistance)
            Line(Point(position.x - lateralDistance, y), Point(position.x + lateralDistance - 1, y))
          else
            Line(Point(position.x - lateralDistance, y), Point(position.x + lateralDistance, y))

        Some(line)
      }
    }

    def turnToLines(maxSearchSpace: Int): Seq[Line] = {
      (Math.max(0, position.y - manhattanDistance) to Math.min(maxSearchSpace, position.y + manhattanDistance))
        .map(y => {
          val lateralDistance = manhattanDistance - Math.abs(position.y - y)
          Line(Point(Math.max(0, position.x - lateralDistance), y), Point(Math.min(maxSearchSpace, position.x + lateralDistance), y))
        })
    }
  }

  def solve(puzzle: String, laterLineAt: Int, maxSearchSpace: Int): Solution[Long, Long] = {
    val sensors = parseInput(puzzle)
    val firstPart = solveFirstPart(sensors, laterLineAt)
    val firstSolution = firstPart.foldLeft(0)((acc, line) => acc + line.length)
    val Point(x, y) = solveSecondPart(sensors, maxSearchSpace)
    Solution(firstSolution, x * 4000000L + y)
  }

  def solveFirstPart(sensors: Array[Sensor], lateralLineAt: Int): Seq[Line] = {
    sensors
      .flatMap(_.lineCutAtWithoutBeacon(lateralLineAt))
      .foldLeft(Seq[Line]()) {
        case (acc, item) =>
          acc.partition(line => line.isTouching(item)) match {
            case (Seq(), noIntersect) => noIntersect.appended(item)
            case (intersects, noIntersect) =>
              val merged = intersects.reduce((acc, item) => acc + item) + item
              noIntersect.appended(merged)
          }
      }
  }

  def solveSecondPart(sensors: Array[Sensor], maxSearchSpace: Int): Point = {
    sensors
      .flatMap(_.turnToLines(maxSearchSpace))
      .groupBy(_.start.y)
      .find {
        case (i, lines) =>
          reduceLines(lines).length != 1
      }
      .map { case (_, lines) => reduceLines(lines) }
      .map {
        case Seq(first, second) if first.end.x + 2 == second.start.x => Point(first.end.x + 1, first.end.y)
      }
      .orNull
  }

  private def reduceLines(lines: Array[Line]): Seq[Line] = {
    lines.foldLeft(Seq[Line]()) {
      case (acc, line) =>
        acc.partition(_.isTouching(line)) match {
          case (Seq(), noTouching) =>
            noTouching.appended(line)
          case (touching, noTouching) =>
            val touched = touching
              .sortBy(_.start.x)
              .appended(line)
              .reduce((a, b) => a + b)
            noTouching.appended(touched)
        }
    }
  }

  def parseInput(puzzle: String): Array[Sensor] =
    puzzle.split("\n")
      .map(parseSensor)

  def parseSensor(line: String): Sensor =
    line.trim match {
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
        val sensor = Point(Integer.parseInt(sensorX), Integer.parseInt(sensorY))
        val beacon = Point(Integer.parseInt(beaconX), Integer.parseInt(beaconY))
        val manhattanDistance = Math.abs(sensor.x - beacon.x) + Math.abs(sensor.y - beacon.y)
        Sensor(sensor, beacon, manhattanDistance)
    }
}
