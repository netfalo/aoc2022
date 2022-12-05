object Day3 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {
    val lines = puzzle.split('\n')
      .map(_.trim())
      .filterNot(_.isBlank())

    Solution(calculateScore(solveFirstPart(lines)), calculateScore(solveSecondPart(lines)))
  }

  def solveFirstPart(lines: Seq[String]): Seq[Char] =
    lines
      .map(line => (line.substring(0, line.length() / 2), line.substring(line.length() / 2)))
      .flatMap {
        case (first, second) => first.find(second.contains(_))
      }

  def solveSecondPart(lines: Seq[String]): Seq[Char] =
    lines
      .grouped(3)
      .flatMap {
        case Seq(first, second, third) => first.find(c => second.contains(c) && third.contains(c))
      }
      .toSeq

  def calculateScore(chars: Seq[Char]): Int =
    chars
      .map(c =>
        if (c.isLower) c - 'a' + 1
        else c - 'A' + 27)
      .sum
}
