class Day20Test extends AnyFlatSpec {
  private val sut = Day20
  private val example =
    """1
      |2
      |-3
      |3
      |-2
      |0
      |4""".stripMargin

  private lazy val puzzle: String = Resource("Day20.txt").content

  "Day20" should "solve example" in {
    sut.solve(example) shouldBe Solution(3, 0)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(0, 0)
  }
}
