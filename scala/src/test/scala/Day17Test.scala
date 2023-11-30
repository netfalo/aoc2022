class Day17Test extends AnyFlatSpec {
  private val sut = Day17
  private val example =
    """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>""".stripMargin

  private lazy val puzzle: String = Resource("Day17.txt").content


  "Day17" should "solve example" in {
    sut.solve(example) shouldBe Solution(3068, 0)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(0, 0)
  }

  it should "drop one" in {
    sut.dropOneRock(0, example, Vector())._2 shouldBe Vector(
      "..####."
    )
  }

  it should "drop two" in {
    val (round1, afterFirst) = sut.dropOneRock(0, example, Vector())
    val (round2, afterSecond) = sut.dropOneRock(round1, example, afterFirst)

    afterSecond.reverse shouldBe Vector(
      "...#...",
      "..###..",
      "...#...",
      "..####."
    )
  }
}
