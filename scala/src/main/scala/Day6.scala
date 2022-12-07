import scala.collection.immutable.Queue
object Day6 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {

    Solution(findStart(puzzle, 4), findStart(puzzle, 14))
  }

  def findStart(puzzle: String, distinctCharsRequired: Int): Int = {
    def findStart(acc: Vector[Char], input: String): Int = {
      if (acc.take(distinctCharsRequired).toSet.size == distinctCharsRequired) acc.size
      else if (input.isEmpty()) -1
      else findStart(acc.prepended(input.head), input.tail)
    }

    findStart(puzzle.take(distinctCharsRequired).reverse.toVector, puzzle.drop(distinctCharsRequired))
  }
}
