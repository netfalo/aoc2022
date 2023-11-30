class Day18Test extends AnyFlatSpec {
  private val sut = Day18
  private val example =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin

  private val example2 =
    """1,1,1
      |2,1,1
      |3,1,1
      |4,1,1
      |5,1,1
      |6,1,1
      |1,2,1
      |2,2,1
      |3,2,1
      |4,2,1
      |5,2,1
      |6,2,1
      |1,3,1
      |2,3,1
      |3,3,1
      |4,3,1
      |5,3,1
      |6,3,1
      |1,1,2
      |2,1,2
      |3,1,2
      |4,1,2
      |5,1,2
      |6,1,2
      |1,2,2
      |6,2,2
      |1,3,2
      |2,3,2
      |3,3,2
      |4,3,2
      |5,3,2
      |6,3,2
      |1,1,3
      |2,1,3
      |3,1,3
      |4,1,3
      |5,1,3
      |6,1,3
      |1,2,3
      |2,2,3
      |3,2,3
      |4,2,3
      |5,2,3
      |6,2,3
      |1,3,3
      |2,3,3
      |3,3,3
      |4,3,3
      |5,3,3
      |6,3,3""".stripMargin

  private lazy val puzzle: String = Resource("Day18.txt").content

  "Day18" should "solve example" in {
    sut.solve(example) shouldBe Solution(64, 58)
  }

  it should "solve example2" in {
    sut.solve(example2) shouldBe Solution(108, 90)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(3522, 2074)
  }
}
