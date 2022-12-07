class Day6Test extends AnyFlatSpec {
  private val sut = Day6
  private val examples = List(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    )

  private lazy val puzzle: String = Resource("Day6.txt").content

  "Day6" should "find start of packet marker in examples" in {
    examples
      .map(Day6.solve(_))
      .map(_.first) shouldBe Seq(7, 5, 6, 10, 11)
  }

  it should "find start of message marker" in {
    Seq(
      ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
      ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
      ("nppdvjthqldpwncqszvftbrmjlhg", 23),
      ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
      ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)
    )
      .foreach {
        case (input, expected) => Day6.solve(input).second shouldBe expected
      }
  }

  it should "solve puzzle" in {
    Day6.solve(puzzle) shouldBe Solution(1356, 2564)
  }

}
