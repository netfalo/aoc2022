import Day11.Monkey

class Day11Test extends AnyFlatSpec {
  private val sut = Day11
  private val example =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin

  private lazy val puzzle: String = Resource("Day11.txt").content


  "Day11" should "solve example" in {
    sut.solve(example) shouldBe Solution(10605, 2713310158L)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(99840, 20683044837L)
  }

  it should "parse monkey" in {
    val text =
      """Monkey 0:
        |  Starting items: 84, 66, 62, 69, 88, 91, 91
        |  Operation: new = old * 11
        |  Test: divisible by 2
        |    If true: throw to monkey 4
        |    If false: throw to monkey 7""".stripMargin

    val Monkey(items, operation, test, _, _) = sut.parseMonkey(text)
    items shouldBe Seq(84, 66, 62, 69, 88, 91, 91)
    operation(7) shouldBe 77
    test(4) shouldBe 4
    test(3) shouldBe 7
  }

  it should "parse another monkey" in {
    val text =
      """Monkey 0:
        |  Starting items: 84, 66, 62, 69, 88, 91, 91
        |  Operation: new = old * old
        |  Test: divisible by 2
        |    If true: throw to monkey 4
        |    If false: throw to monkey 7""".stripMargin

    val Monkey(items, operation, test, _, _) = sut.parseMonkey(text)
    items shouldBe Seq(84, 66, 62, 69, 88, 91, 91)
    operation(7) shouldBe 49
    test(4) shouldBe 4
    test(3) shouldBe 7
  }

  it should "do one round" in {
    val (monkeys, commonDenominator) = sut.parseInput(example)
    val afterOneRound = sut.doMonkeyBusiness(monkeys, sut.inspectItem, commonDenominator)

    afterOneRound.map(_.items) shouldBe Vector(
      Vector(20, 23, 27, 26),
      Vector(2080, 25, 167, 207, 401, 1046),
      Vector(),
      Vector()
    )
  }

  it should "do 20 round" in {
    val (monkeys, commonDenominator) = sut.parseInput(example)
    val afterOneRound = sut.doNRound(monkeys, 20, sut.inspectItem, commonDenominator)

    afterOneRound.map(_.items) shouldBe Vector(
      Vector(10, 12, 14, 26, 34),
      Vector(245, 93, 53, 199, 115),
      Vector(),
      Vector()
    )
  }

  it should "do 10000 round" in {
    val (monkeys,commonDenominator) = sut.parseInput(example)
    val expected = Vector(20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
      .map(sut.doNRound(monkeys, _, sut.inspectWithoutWorry, commonDenominator))
      .map(_.map(_.itemsInspected))

    expected shouldBe Vector(
      Vector(99, 97, 8, 103),
      Vector(5204, 4792, 199, 5192),
      Vector(10419, 9577, 392, 10391),
      Vector(15638, 14358, 587, 15593),
      Vector(20858, 19138, 780, 20797),
      Vector(26075, 23921, 974, 26000),
      Vector(31294, 28702, 1165, 31204),
      Vector(36508, 33488, 1360, 36400),
      Vector(41728, 38268, 1553, 41606),
      Vector(46945, 43051, 1746, 46807),
      Vector(52166, 47830, 1938, 52013)
    )
  }
}
