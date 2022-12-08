import scala.annotation.tailrec

object Day8 extends Problem[Int, Int] {

  def parseLines(input: String): Vector[Vector[Int]] = {
    input
      .split("\n")
      .map(_.trim())
      .filterNot(_.isBlank())
      .map(_.toCharArray.map(_.toInt - '0'.toInt).toVector)
      .toVector
  }

  override def solve(puzzle: String): Solution[Int, Int] = {
    val matrix = parseLines(puzzle)

    Solution(solveFirstPart(matrix), visitMatrix2(matrix))
  }

  def solveFirstPart(matrix: Vector[Vector[Int]]): Int = {
    visitMatrix(matrix)
      .map(_.sum)
      .sum
  }

  def visitMatrix(matrix: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    val fromLeft = matrix.map(visitRow(_))
    val fromRight = matrix.map(x => visitRow(x.reverse).reverse)

    val transposed = matrix.transpose
    val fromAbove = transposed.map(visitRow(_))
    val fromBelow = transposed.map(x => visitRow(x.reverse).reverse)

    def merge(left: Vector[Vector[Int]], right: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      left
        .zip(right)
        .map {
          case (l, r) => l.zip(r).map { case (x, y) => if (x == 1 || y == 1) 1 else 0 }
        }
    }

    val normalMerged = merge(fromLeft, fromRight)
    val transposeMerged = merge(fromAbove, fromBelow).transpose

    merge(normalMerged, transposeMerged)
  }

  def visitRow(row: Vector[Int]): Vector[Int] = {
    def visitRow(max: Int, row: Vector[Int]): Vector[Int] = {
      if (row.isEmpty)
        Vector()
      else
        visitRow(if (max < row.head) row.head else max, row.tail)
          .prepended(if (max < row.head) 1 else 0)
    }

    visitRow(row.head, row.tail)
      .prepended(1)
  }

  def visitMatrix2(row: Vector[Vector[Int]]): Int = {
    (for {
      x <- row.indices
      y <- row.indices
    } yield scenicScoreOf(row, x, y)
      )
      .max
  }

  def scenicScoreOf(matrix: Vector[Vector[Int]], x: Int, y: Int): Int = {
    val tree = matrix(y)(x)
    val (left, right) = matrix(y).splitAt(x)
    val transposed = matrix.transpose
    val (top, bottom) = transposed(x).splitAt(y)

    val lookUp = top.reverse.takeWhile(_ < tree).size
    val lookLeft = left.reverse.takeWhile(_ < tree).size
    val lookRight = right.tail.takeWhile(_ < tree).size
    val lookDown = bottom.tail.takeWhile(_ < tree).size

    (lookUp + (if (lookUp < top.size) 1 else 0)) *
      (lookLeft + (if (lookLeft < left.size) 1 else 0)) *
      (lookRight + (if (lookRight < right.tail.size) 1 else 0)) *
      (lookDown + (if (lookDown < bottom.tail.size) 1 else 0))
  }
}
