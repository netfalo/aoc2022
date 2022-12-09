import javax.print.attribute.standard.PDLOverrideSupported

class Day9Test extends AnyFlatSpec {

  import Day9._

  private val sut = Day9
  private val example =
    """
    R 4
    U 4
    L 3
    D 1
    R 4
    D 1
    L 5
    R 2"""

    private val example2 =
      """
        |R 5
        |U 8
        |L 8
        |D 3
        |R 17
        |D 10
        |L 25
        |U 20
        |""".stripMargin


  private lazy val puzzle: String = Resource("Day9.txt").content

  "Day9" should "solve example" in {
    sut.solve(example) shouldBe Solution(13, 1)
  }

  it should "solve second example" in {
    sut.simulateMovement(sut.parseMovements(example2), 10).size shouldBe 36
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(6522, 2717)
  }

  it should "parse movements" in {
    sut.parseMovements(example) shouldBe Array(
      (Right, 4),
      (Up, 4),
      (Left, 3),
      (Down, 1),
      (Right, 4),
      (Down, 1),
      (Left, 5),
      (Right, 2)
    )
  }

  it should "simulate movements" in {
    sut.simulateMovement(sut.parseMovements(example), 2) shouldBe Set(
      Position(0, 0),
      Position(1, 0),
      Position(2, 0),
      Position(3, 0),
      Position(4, 1),
      Position(4, 2),
      Position(4, 3),
      Position(3, 4),
      Position(2, 4),
      Position(3, 3),
      Position(3, 2),
      Position(2, 2),
      Position(1, 2),
    )
  }

  it should "do one step for 11 knots" in {
    val (finalPositions, lastKnotHistory) = sut.simulateMovement(Seq.fill(11)(Position(0, 0)), Seq(Set(Position(0, 0))), Array((Right, 4)))

    finalPositions shouldBe Vector(
      Position(4, 0),
      Position(3, 0),
      Position(2, 0),
      Position(1, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0))
  }

  it should "do two step for 11 knots" in {
    val (finalPositions, lastKnotHistory) = sut.simulateMovement(Seq.fill(11)(Position(0, 0)), Seq(Set(Position(0, 0))), Array((Right, 4), (Up, 4)))

    finalPositions shouldBe Vector(
      Position(4, 4),
      Position(4, 3),
      Position(4, 2),
      Position(3, 2),
      Position(2, 2),
      Position(1, 1),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0),
      Position(0, 0))
  }

}
