class Day13Test extends AnyFlatSpec {
  private val sut = Day13
  private val example =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin
  private lazy val puzzle: String = Resource("Day13.txt").content


  "Day13" should "solve example" in {
    sut.solve(example) shouldBe Solution(13, 140)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(6369, 25800)
  }

  it should "find head and tail of packet" in {
    sut.findHeadAndTail("[1,1,3,1,1]") shouldBe ("1", "[1,3,1,1]")
    sut.findHeadAndTail("[1]") shouldBe ("1", "[]")
    sut.findHeadAndTail("[[1],[2,3,4]]") shouldBe ("[1]", "[[2,3,4]]")
    sut.findHeadAndTail("[[10],[2,3,4]]") shouldBe ("[10]", "[[2,3,4]]")
    sut.findHeadAndTail("[[1],[20,3,4]]") shouldBe ("[1]", "[[20,3,4]]")
    sut.findHeadAndTail("[[2,3,4]]") shouldBe ("[2,3,4]", "[]")
    sut.findHeadAndTail("[[]]") shouldBe ("[]", "[]")
    sut.findHeadAndTail("[]") shouldBe ("", "[]")
  }

  it should "compare packets" in {
    sut.comparePackets("[1,1,3,1,1]", "[1,1,5,1,1]") shouldBe -1
    sut.comparePackets("[[1],[2,3,4]]", "[[1],4]") shouldBe -1
    sut.comparePackets("[9]", "[[8,7,6]]") shouldBe 1
    sut.comparePackets("[[4,4],4,4]", "[[4,4],4,4,4]") shouldBe -1
    sut.comparePackets("[7,7,7,7]", "[7,7,7]") shouldBe 1
    sut.comparePackets("[]", "[3]") shouldBe -1
    sut.comparePackets("[[[]]]", "[[]]") shouldBe 1
    sut.comparePackets("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]") shouldBe 1
  }
}
