class Day3Test extends AnyFlatSpec {
  private val sut = Day3
  private val example = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

  private lazy val puzzle: String = Resource("Day3.txt").content

  "Day3" should "solve example" in {
    Day3.solve(example) shouldBe Solution(157, 70)
  }

  it should "solve puzzle" in {
    Day3.solve(puzzle) shouldBe Solution(8088, 2522)
  }
}
