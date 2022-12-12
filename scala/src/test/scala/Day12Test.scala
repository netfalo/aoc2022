class Day12Test extends AnyFlatSpec {
  private val sut = Day12
  private val example =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin
  private lazy val puzzle: String = Resource("Day12.txt").content


  "Day12" should "solve example" in {
    sut.solve(example) shouldBe Solution(31, 29)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(420, 414)
  }

  it should "parse wall" in {
    sut.parseWall(example) shouldBe(
      Vector(
        "aabqponm",
        "abcryxxl",
        "accszzxk",
        "acctuvwj",
        "abdefghi"),
      (0, 0),
      (5, 2))
  }
}
