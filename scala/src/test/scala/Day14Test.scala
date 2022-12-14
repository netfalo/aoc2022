import Day14.{dropOneSandParticle, nextMove}

class Day14Test extends AnyFlatSpec {
  private val sut = Day14
  private val example =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin
  private lazy val puzzle: String = Resource("Day14.txt").content


  "Day14" should "solve example" in {
    sut.solve(example) shouldBe Solution(24, 93)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(592, 0)
  }

  it should "parse lines" in {
    sut.parseLines(example) shouldBe(
      Point(494, 0),
      Point(9, 9),
      Vector(
        Line(Point(4, 4), Point(4, 6)),
        Line(Point(4, 6), Point(2, 6)),
        Line(Point(9, 4), Point(8, 4)),
        Line(Point(8, 4), Point(8, 9)),
        Line(Point(8, 9), Point(0, 9))
      )
    )
  }

  it should "generate map" in {
    val lines = Vector(
      Line(Point(4, 4), Point(4, 6)),
      Line(Point(4, 6), Point(2, 6)),
      Line(Point(9, 4), Point(8, 4)),
      Line(Point(8, 4), Point(8, 9)),
      Line(Point(8, 9), Point(0, 9))
    )

    val expected = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      "........#.",
      "#########."
    )

    sut.generateMap(Point(9, 9), lines) shouldBe expected
  }

  it should "drop one sand" in {
    val source = Point(500, 0)
    val offset = Point(494, 0)
    val max = Point(9, 9)
    val points = Vector(
      Line(Point(4, 4), Point(4, 6)),
      Line(Point(4, 6), Point(2, 6)),
      Line(Point(9, 4), Point(8, 4)),
      Line(Point(8, 4), Point(8, 9)),
      Line(Point(8, 9), Point(0, 9))
    )

    val expected = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      "......o.#.",
      "#########."
    )

    sut.dropOneSandParticle(source - offset, sut.generateMap(max, points)) shouldBe Some(expected)
  }

  it should "drop second sand to test right movement" in {
    val input = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      "......o.#.",
      "#########."
    )

    val expected = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      ".....oo.#.",
      "#########."
    )

    sut.dropOneSandParticle(Point(6, 0), input) shouldBe Some(expected)
  }

  it should "match dropped sand pattern after 5" in {
    val input = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      "........#.",
      "#########."
    )
    val expected = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "......o.#.",
      "....oooo#.",
      "#########."
    )

    (0 until 5)
      .foldLeft(input) {
        case (acc, _) => sut.dropOneSandParticle(Point(6, 0), acc).get
      } shouldBe expected
  }


  it should "drop sand until it starts pouring down" in {
    val input = Vector(
      "..........",
      "..........",
      "..........",
      "..........",
      "....#...##",
      "....#...#.",
      "..###...#.",
      "........#.",
      "........#.",
      "#########."
    )

    val expected = Vector(
      "..........",
      "..........",
      "......o...",
      ".....ooo..",
      "....#ooo##",
      "...o#ooo#.",
      "..###ooo#.",
      "....oooo#.",
      ".o.ooooo#.",
      "#########."
    )

    sut.dropUntilItPoursDown(Point(6, 0), input) shouldBe expected
  }

  it should "pour down" in {
    val input = Vector(
      "..........",
      "..........",
      "......o...",
      ".....ooo..",
      "....#ooo##",
      "...o#ooo#.",
      "..###ooo#.",
      ".....ooo#.",
      ".o..oooo#.",
      "#########."
    )

    sut.nextMove(Point(1, 7), input) shouldBe Some(Point(0, 8))
    sut.nextMove(Point(0, 8), input) shouldBe Some(Point(-1, 9))
  }
}
