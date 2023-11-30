import scala.annotation.tailrec

object Day17 extends Problem[Int, Int] {

  case class Shape(topLeft: Point, form: Vector[String]) {
    def fall: Shape = Shape(topLeft + Point(0, -1), form)

    def left: Shape = Shape(topLeft + Point(-1, 0), form)

    def right: Shape = Shape(topLeft + Point(1, 0), form)
  }

  private val shapes: Vector[Point => Shape] = Vector(
    point => Shape(point + Point(0, 1), Vector("####")),
    point => Shape(point + Point(0, 3), Vector(".#.", "###", ".#.")),
    point => Shape(point + Point(0, 3), Vector("..#", "..#", "###")),
    point => Shape(point + Point(0, 4), Vector("#", "#", "#", "#")),
    point => Shape(point + Point(0, 2), Vector("##", "##")),
  )

  override def solve(puzzle: String): Solution[Int, Int] = Solution(0, 0)

  def addRockToChamber(chamber: Vector[String], rock: Shape): Vector[String] =
    chamber
      .zipWithIndex
      .foldLeft(Vector[String]()) {
        case (acc, (line, i)) if rock.topLeft.y - rock.form.length + 1 <= i && i <= rock.topLeft.y =>

          val rockLine = rock.form(i - (rock.topLeft.y - rock.form.length + 1))
          val result = line
            .zipWithIndex
            .map {
              case (c, i) if i < rock.topLeft.x || rock.topLeft.x + rockLine.length <= i => c
              case (c, i) =>
                if (rockLine(i - rock.topLeft.x) == '#' || c == '#') '#'
                else '.'
            }
            .mkString

          acc.appended(result)
        case (acc, (line, _)) =>
          acc.appended(line)
      }
      .takeWhile(_.contains('#'))

  def printRockAndChamber(rock: Shape, chamber: Vector[String]): Unit = {
    val prettyChamber = addRockToChamber(chamber, rock)
      .reverse
      .map(_.prepended('|').appended('|'))
      .appended("+-------+")
      .mkString("\n")
      .appended('\n')

    println(prettyChamber)

  }

  def dropOneRock(round: Int, jets: String, chamber: Vector[String]): (Int, Vector[String]) = {
    @tailrec
    def dropOneRock(round: Int, rock: Shape, chamber: Vector[String]): (Int, Vector[String]) = {
      val pushedRock: Shape = pushRockSideways(jets(round % jets.length), chamber, rock)

      dropOneLevel(pushedRock, chamber) match {
        case Right(rok) => (round + 2, addRockToChamber(chamber, rok))
        case Left(rok) => dropOneRock(round + 2, rok, chamber)
      }
    }

    val topRow = chamber.lastIndexWhere(_.contains('#'))
    val spawningPosition =
      Point(2, (if (topRow != -1) topRow else 0) + 2)
    val rock = shapes(round % shapes.length)(spawningPosition)

    dropOneRock(round, rock, chamber.appendedAll(Vector.fill(3 + rock.form.length)(Seq.fill(7)('.').mkString)))
  }

  private def dropOneLevel(rock: Shape, chamber: Vector[String]): Either[Shape, Shape] = {
    val fallenRock = rock.fall
    if (fallenRock.topLeft.y == 0) Right(fallenRock)
    else {
      val fallingCollision = fallenRock.form.last
        .zip(chamber(fallenRock.topLeft.y - fallenRock.form.length + 1).substring(fallenRock.topLeft.x, fallenRock.form.head.length))
        .exists { case (l, r) => l == '#' && r == '#' }
      if (fallingCollision) Right(rock)
      else Left(fallenRock)
    }
  }

  private def pushRockSideways(jet: Char, chamber: Vector[String], rock: Shape) = {
    val pushedRock = jet match {
      case '<' if rock.topLeft.x > 0 => rock.left
      case '>' if rock.topLeft.x + rock.form.head.length < chamber.head.length => rock.right
      case _ => rock
    }
    val updatedRock =
      if (rock == pushedRock) rock
      else {
        val pushWillCollide = chamber
          .slice(pushedRock.topLeft.y, pushedRock.form.length)
          .map(line => line.slice(pushedRock.topLeft.x, pushedRock.form.head.length))
          .zip(pushedRock.form)
          .exists { case (l, r) => l.zip(r).exists { case (x, y) => x == '#' && y == '#' } }
        if (pushWillCollide) rock
        else pushedRock
      }
    updatedRock
  }
}
