import scala.annotation.tailrec

object Day9 extends Problem[Int, Int] {
  case class Position(x: Int, y: Int) {
    def isTouching(other: Position): Boolean = {
      Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
    }
  }

  override def solve(puzzle: String): Solution[Int, Int] = {
    val movements = parseMovements(puzzle)

    Solution(simulateMovement(movements, 2).size, simulateMovement(movements, 10).size)
  }

  def simulateMovement(movements: Array[(Direction, Int)], value: Int): Set[Position] = {
    val (_, history) = simulateMovement(Vector.fill(value)(Position(0, 0)), Vector.fill(value)(Set(Position(0, 0))), movements)

    history.last
  }

  @tailrec
  def simulateMovement(knots: Seq[Position], history: Seq[Set[Position]], movements: Array[(Direction, Int)]): (Seq[Position], Seq[Set[Position]]) = {
    if (movements.isEmpty) (knots, history)
    else {
      val head = movements.head
      val (updatedKnots, stepsDoneInThisRound) = doMovement(knots, head)
      val updatedHistory = history.zip(stepsDoneInThisRound).map { case (x, y) => x ++ y }
      simulateMovement(updatedKnots, updatedHistory, movements.tail)
    }
  }

  def doMovement(knots: Seq[Position], move: (Day9.Direction, Int)): (Seq[Position], Seq[Set[Position]]) = {
    @tailrec
    def doOneStep(acc: Seq[Set[Position]], knots: Seq[Position], direction: Direction, step: Int): (Seq[Position], Seq[Set[Position]]) = {
      if (step == 0) (knots, acc)
      else {
        val newHeadPosition = direction match {
          case Up => Position(knots.head.x, knots.head.y + 1)
          case Left => Position(knots.head.x - 1, knots.head.y)
          case Right => Position(knots.head.x + 1, knots.head.y)
          case Down => Position(knots.head.x, knots.head.y - 1)
        }
        val updatedKnots = knots.tail
          .foldLeft(Vector(newHeadPosition)) {
            case (acc, tail) =>
              val head = acc.last
              if (head.isTouching(tail)) acc :+ tail
              else {
                if (head.x == tail.x && head.y > tail.y) acc :+ Position(tail.x, tail.y + 1)
                else if (head.x == tail.x && head.y < tail.y) acc :+ Position(tail.x, tail.y - 1)
                else if (head.x > tail.x && head.y == tail.y) acc :+ Position(tail.x + 1, tail.y)
                else if (head.x < tail.x && head.y == tail.y) acc :+ Position(tail.x - 1, tail.y)
                else if (head.x < tail.x && head.y < tail.y) acc :+ Position(tail.x - 1, tail.y - 1)
                else if (head.x > tail.x && head.y < tail.y) acc :+ Position(tail.x + 1, tail.y - 1)
                else if (head.x < tail.x && head.y > tail.y) acc :+ Position(tail.x - 1, tail.y + 1)
                else acc :+ Position(tail.x + 1, tail.y + 1)
              }
          }

        val updatedHistory = acc.zip(updatedKnots).map { case (x, y) => x + y }
        doOneStep(updatedHistory, updatedKnots, direction, step - 1)
      }
    }

    move match {
      case (direction, step) => doOneStep(knots.map(Set(_)), knots, direction, step)
    }
  }

  def parseMovements(input: String): Array[(Direction, Int)] =
    input
      .split("\n")
      .filterNot(_.isBlank)
      .map(_.trim)
      .map {
        case s"$dir $step" =>
          dir match {
            case "U" => (Up, Integer.parseInt(step))
            case "D" => (Down, Integer.parseInt(step))
            case "R" => (Right, Integer.parseInt(step))
            case "L" => (Left, Integer.parseInt(step))
          }
      }

  sealed trait Direction

  object Down extends Direction

  object Up extends Direction

  object Left extends Direction

  object Right extends Direction
}
