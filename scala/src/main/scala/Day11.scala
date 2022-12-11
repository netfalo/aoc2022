import scala.annotation.tailrec

object Day11 extends Problem[Long, Long] {
  case class Monkey(items: Seq[Long], operation: Long => Long, test: Long => Long, itemsInspected: Long, testValue: Long)

  override def solve(puzzle: String): Solution[Long, Long] = {
    val (monkeys, commonDenominator) = parseInput(puzzle)

    Solution(calculateMonkeyBusiness(monkeys, inspectItem, 20, commonDenominator), calculateMonkeyBusiness(monkeys, inspectWithoutWorry, 10000, commonDenominator))
  }

  def calculateMonkeyBusiness(monkeys: Seq[Monkey], inspectionMethod: (Long, Monkey, Long) => (Long, Long), n: Long, commonDenominator: Long): Long = {
    doNRound(monkeys, n, inspectionMethod, commonDenominator)
      .sortBy(_.itemsInspected * -1)
      .take(2)
      .foldLeft(1L) {
        case (acc, item) => acc * item.itemsInspected
      }
  }

  def parseInput(puzzle: String): (Seq[Monkey], Long) = {
    val monkeys = puzzle
      .split("\n\n")
      .map(parseMonkey)
      .toVector

    val commonDenominator = monkeys
      .foldLeft(1L) {
        case (acc, item) => acc * item.testValue
      }

    (monkeys, commonDenominator)
  }

  def parseMonkey(monkey: String): Monkey = {
    val traits = monkey.split("\n").tail.map(_.trim)
    val items = traits(0).split(": ")(1).split(", ").map(Integer.parseInt).map(_.toLong).toList

    def operation = traits(1) match {
      case s"Operation: new = old $op old" =>
        op match {
        case "*" => (old: Long) => old * old
        case "+" => (old: Long) => old + old
      }
      case s"Operation: new = old $op $right" =>
        val r = Integer.parseInt(right)
        op match {
          case "*" => (old: Long) => old * r
          case "+" => (old: Long) => old + r
        }
    }

    val t = traits(3) match {
      case s"If true: throw to monkey $monkey" => Integer.parseInt(monkey).toLong
    }

    val f = traits(4) match {
      case s"If false: throw to monkey $monkey" => Integer.parseInt(monkey).toLong
    }
    val divisibleBy: Long = traits(2) match {
      case s"Test: divisible by $num" => Integer.parseInt(num).toLong
    }

    def test = (worryLevel: Long) =>
      if (worryLevel % divisibleBy == 0) t
      else f


    Monkey(items, operation, test, 0, divisibleBy)
  }

  @tailrec
  def doNRound(monkeys: Seq[Monkey], n: Long, inspectionMethod: (Long, Monkey, Long) => (Long, Long), commonDenominator: Long): Seq[Monkey] = {
    if (n == 0) monkeys
    else {
      doNRound(doMonkeyBusiness(monkeys, inspectionMethod, commonDenominator), n - 1, inspectionMethod, commonDenominator)
    }
  }

  def doMonkeyBusiness(monkeys: Seq[Monkey], inspectionMethod: (Long, Monkey, Long) => (Long, Long), commonDenominator: Long): Seq[Monkey] = {
    @tailrec
    def doMonkeyBusiness(monkeys: Seq[Monkey], cntr: Int): Seq[Monkey] = {
      if (cntr == monkeys.size) monkeys
      else {
        val current = monkeys(cntr)
        val thrownItems = current.items
          .map(inspectionMethod(_, current, commonDenominator))
          .groupBy(_._1)

        val monkeysAfterInspection = monkeys.zipWithIndex
          .map {
            case (Monkey(items, op, test, itemsInspected, m), index) if index == cntr => Monkey(Seq(), op, test, itemsInspected + items.size, m)
            case (Monkey(items, operation, test, itemsInspected, m), index) if thrownItems.contains(index) =>
              Monkey(items.appendedAll(thrownItems(index).map(_._2)), operation, test, itemsInspected, m)
            case (monkey, _) => monkey
          }

        doMonkeyBusiness(monkeysAfterInspection, cntr + 1)
      }
    }

    doMonkeyBusiness(monkeys, 0)
  }

  def inspectItem(item: Long, monkey: Monkey, commonDenominator: Long): (Long, Long) = {
    val increasedWorryLevel = monkey.operation(item) % commonDenominator
    val decreasedBecauseItDidntBroke = increasedWorryLevel / 3

    (monkey.test(decreasedBecauseItDidntBroke), decreasedBecauseItDidntBroke)
  }

  def inspectWithoutWorry(item: Long, monkey: Monkey, commonDenominator: Long): (Long, Long) = {
    val increasedWorryLevel = monkey.operation(item) % commonDenominator

    (monkey.test(increasedWorryLevel), increasedWorryLevel)
  }
}
