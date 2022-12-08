class Day8Test extends AnyFlatSpec {
  private val sut = Day8
  private val example =
    """
30373
25512
65332
33549
35390
"""

  private lazy val puzzle: String = Resource("Day8.txt").content

  "Day8" should "solve example" in {
    sut.solve(example) shouldBe Solution(21, 8)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(1698, 672280)
  }

  it should "parse lines" in {
    val expected = Vector(
      Vector(3,0,3,7,3),
      Vector(2,5,5,1,2),
      Vector(6,5,3,3,2),
      Vector(3,3,5,4,9),
      Vector(3,5,3,9,0)
    )

    sut.parseLines(example) shouldBe expected
  }

  it should "visit matrix" in {
    val input = Vector(
      Vector(3,0,3,7,3),
      Vector(2,5,5,1,2),
      Vector(6,5,3,3,2),
      Vector(3,3,5,4,9),
      Vector(3,5,3,9,0)
    )
    val expected = Vector(
      Vector(1,1,1,1,1),
      Vector(1,1,1,0,1),
      Vector(1,1,0,1,1),
      Vector(1,0,1,0,1),
      Vector(1,1,1,1,1)
    )

    sut.visitMatrix(input) shouldBe expected
  }

  it should "calculate scenic score from" in {
    val input = Vector(
      Vector(3,0,3,7,3),
      Vector(2,5,5,1,2),
      Vector(6,5,3,3,2),
      Vector(3,3,5,4,9),
      Vector(3,5,3,9,0)
    )

    sut.scenicScoreOf(input, 2, 1) shouldBe 4
    sut.scenicScoreOf(input, 2, 3) shouldBe 8
 }
}
