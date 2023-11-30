class Day19Test extends AnyFlatSpec {
  private val sut = Day19
  private val example =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

  "Day19" should "solve example" in {
    sut.solve(example) shouldBe Solution(33, 0)
  }
}
