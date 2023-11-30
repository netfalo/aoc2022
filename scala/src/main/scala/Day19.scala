import scala.annotation.tailrec

object Day19 extends Problem[Int, Int] {
  sealed trait Element

  object Ore extends Element

  object Clay extends Element

  object Obsidian extends Element

  object Geode extends Element

  case class Cost(ore: Int, clay: Int, obsidian: Int, geode: Int)

  case class Treasury(ore: Int, clay: Int, obsidian: Int, geode: Int) {
    def hasEnoughOf(cost: Cost): Boolean = {
      cost.ore <= ore && cost.clay <= clay && cost.obsidian <= obsidian && cost.geode <= geode
    }

    def pay(cost: Cost): Treasury = {
      require(hasEnoughOf(cost))
      Treasury(ore - cost.ore, clay - cost.clay, obsidian - cost.obsidian, geode - cost.geode)
    }

    def collect(c: (Int, Int, Int, Int)): Treasury =
      Treasury(c._1 + ore, c._2 + clay, c._3 + obsidian, c._4 + geode)
  }

  case class Blueprint(id: Int, robotCosts: Seq[(String, Cost)]) {

  }

  override def solve(puzzle: String): Solution[Int, Int] = {
    val blueprints = parseBluePrints(puzzle)
    val firstSolution = solveFirstPart(blueprints)

    Solution(firstSolution, 0)
  }

  def solveFirstPart(blueprints: Seq[Blueprint]): Int = {
    blueprints
      .map(blueprint => (blueprint, run(blueprint)))
      .map(b => b._1.id * b._2.geode)
      .sum
  }

  def parseBluePrints(puzzle: String): Seq[Blueprint] =
    puzzle
      .split('\n')
      .toVector
      .map(parseBluePrint)

  def parseBluePrint(line: String): Blueprint = line match {
    case s"Blueprint $id: Each ore robot costs $oreRobotOreCost ore. Each clay robot costs $clayRobotOreCost ore. Each obsidian robot costs $obsidianRobotOreCost ore and $obsidianRobotClayCost clay. Each geode robot costs $geodeRobotOreCost ore and $geodeRobotObsidianCost obsidian." =>
      Blueprint(Integer.parseInt(id),
        Seq(("ore", Cost(Integer.parseInt(oreRobotOreCost), 0, 0, 0)),
          ("clay", Cost(Integer.parseInt(clayRobotOreCost), 0, 0, 0)),
          ("obsidian", Cost(Integer.parseInt(obsidianRobotOreCost), Integer.parseInt(obsidianRobotClayCost), 0, 0)),
          ("geode", Cost(Integer.parseInt(geodeRobotOreCost), 0, Integer.parseInt(geodeRobotObsidianCost), 0))))
  }

  def run(blueprint: Blueprint): Treasury = {
    def rec(time: Int, robots: (Int, Int, Int, Int), treasury: Treasury): Treasury = {
      if (time > 24) treasury
      else {
        blueprint
          .robotCosts
          .filter(robotCost => treasury.hasEnoughOf(robotCost._2))
          .appended(("wait", Cost(0, 0, 0, 0)))
          .map {
            case (ore, cost) if ore == "ore" => rec(time + 1, (robots._1 + 1, robots._2, robots._3, robots._4), treasury.pay(cost).collect(robots))
            case (clay, cost) if clay == "clay" => rec(time + 1, (robots._1, robots._2 + 1, robots._3, robots._4), treasury.pay(cost).collect(robots))
            case (obsidian, cost) if obsidian == "obsidian" => rec(time + 1, (robots._1, robots._2, robots._3 + 1, robots._4), treasury.pay(cost).collect(robots))
            case (geode, cost) if geode == "geode" => rec(time + 1, (robots._1, robots._2, robots._3, robots._4 + 1), treasury.pay(cost).collect(robots))
            case _ => rec(time + 1, robots, treasury.collect(robots))
          }
          .maxBy(_.geode)
      }
    }

    rec(1, (1, 0, 0, 0), Treasury(0, 0, 0, 0))
  }
}
