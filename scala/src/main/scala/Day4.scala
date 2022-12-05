object Day4 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {
    val lines = parseLines(puzzle)

    Solution((solveFirstPart(lines)), (solveSecondPart(lines)))
  }

  def parseLines(input: String): Array[Array[Int]] =
    input.split('\n')
      .map(_.trim())
      .filterNot(_.isBlank())
      .map(_.split(","))
      .map(line => line.flatMap(_.split("-")).map(_.toInt))

  def solveFirstPart(lines: Array[Array[Int]]): Int = {
    lines
      .filter {
        case (Array(firstElfLower,firstElfUpper, secondElfLower, secondElfUpper)) =>
          firstElfLower <= secondElfLower && firstElfUpper >= secondElfUpper ||
          firstElfLower >= secondElfLower && firstElfUpper <= secondElfUpper
      }
      .size
  }

  def solveSecondPart(lines: Array[Array[Int]]): Int =
    lines
      .filter {
        case (Array(firstElfLower,firstElfUpper, secondElfLower, secondElfUpper)) =>
          firstElfLower <= secondElfLower && firstElfUpper >= secondElfLower ||
          secondElfLower <= firstElfLower && secondElfUpper >= firstElfLower
      }
      .size
}
