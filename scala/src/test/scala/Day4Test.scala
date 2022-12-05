class Day4Test extends AnyFlatSpec {
  private val sut = Day4
  private val example = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

  private lazy val puzzle: String = Resource("Day4.txt").content

  "Day4" should "solve example" in {
    Day4.solve(example) shouldBe Solution(2, 4)
  }

  it should "solve puzzle" in {
    Day4.solve(puzzle) shouldBe Solution(459, 779)
  }
}
