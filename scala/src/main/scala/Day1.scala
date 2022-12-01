class Day1 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {
    val carriedCalories = puzzle
      .split("\n\n")
      .map(_.split("\n")
        .filterNot(_.isBlank)
        .map(_.trim)
        .map(_.toInt)
        .toSeq)
      .map(_.sum)
      .toSeq

    Solution(solveFirstPart(carriedCalories), solveSecondPart(carriedCalories))
  }

  private def solveFirstPart(carriedCalories: Seq[Int]): Int =
    carriedCalories
      .max

  private def solveSecondPart(carriedCalories: Seq[Int]): Int =
    carriedCalories
      .sorted
      .reverse
      .take(3)
      .sum
}
