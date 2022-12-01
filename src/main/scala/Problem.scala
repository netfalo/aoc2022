trait Problem[T, U] {
  def solve(puzzle: String): Solution[T, U]
}

