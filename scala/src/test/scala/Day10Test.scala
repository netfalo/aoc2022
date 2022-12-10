class Day10Test extends AnyFlatSpec {

  import Day10._

  private val sut = Day10
  private val smallExample =
    """
      |noop
      |addx 3
      |addx -5
      |""".stripMargin
  private val largeExample =
    """
      |addx 15
      |addx -11
      |addx 6
      |addx -3
      |addx 5
      |addx -1
      |addx -8
      |addx 13
      |addx 4
      |noop
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx -35
      |addx 1
      |addx 24
      |addx -19
      |addx 1
      |addx 16
      |addx -11
      |noop
      |noop
      |addx 21
      |addx -15
      |noop
      |noop
      |addx -3
      |addx 9
      |addx 1
      |addx -3
      |addx 8
      |addx 1
      |addx 5
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx -36
      |noop
      |addx 1
      |addx 7
      |noop
      |noop
      |noop
      |addx 2
      |addx 6
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx 1
      |noop
      |noop
      |addx 7
      |addx 1
      |noop
      |addx -13
      |addx 13
      |addx 7
      |noop
      |addx 1
      |addx -33
      |noop
      |noop
      |noop
      |addx 2
      |noop
      |noop
      |noop
      |addx 8
      |noop
      |addx -1
      |addx 2
      |addx 1
      |noop
      |addx 17
      |addx -9
      |addx 1
      |addx 1
      |addx -3
      |addx 11
      |noop
      |noop
      |addx 1
      |noop
      |addx 1
      |noop
      |noop
      |addx -13
      |addx -19
      |addx 1
      |addx 3
      |addx 26
      |addx -30
      |addx 12
      |addx -1
      |addx 3
      |addx 1
      |noop
      |noop
      |noop
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop
      |
      |""".stripMargin


  private val expectedExampleImage =
    """##..##..##..##..##..##..##..##..##..##..
      |###...###...###...###...###...###...###.
      |####....####....####....####....####....
      |#####.....#####.....#####.....#####.....
      |######......######......######......####
      |#######.......#######.......#######.....""".stripMargin

  private lazy val puzzle: String = Resource("Day10.txt").content

  "Day10" should "parse small example" in {
    sut.parseProgram(smallExample) shouldBe Vector(
      Noop,
      Addx(3),
      Addx(-5)
    )
  }

  it should "run small example" in {
    sut.runProgram(sut.parseProgram(smallExample)) shouldBe Vector(
      (0, 1),
      (1, 1),
      (2, 1),
      (3, 4),
      (4, 4)
    )
  }

  it should "run large example" in {
    val result = sut.runProgram(sut.parseProgram(largeExample))

    result(19)._2 * 20 shouldBe 420
    result(59)._2 * 60 shouldBe 1140
    result(99)._2 * 100 shouldBe 1800
    result(139)._2 * 140 shouldBe 2940
    result(179)._2 * 180 shouldBe 2880
    result(219)._2 * 220 shouldBe 3960
  }

  it should "solve large example" in {
    val program = sut.parseProgram(largeExample)
    val result = sut.runProgram(program)
    sut.calculateSolutionForFirstPart(result) shouldBe 13140
    sut.renderImage(result).mkString("\n") shouldBe expectedExampleImage
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(14560, "EKRHEPUZ")
  }

  it should "decode R" in {
    inside(sut.decodeCharacter("###.\n#..#\n#..#\n###.\n#..#\n###.")) {
      case (confidence, char) =>
        char shouldBe 'P'
        Math.round(confidence * 100) / 100.0 shouldBe 0.9
    }
  }
}
