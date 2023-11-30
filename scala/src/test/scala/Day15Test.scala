import Day15.Sensor

class Day15Test extends AnyFlatSpec {
  private val sut = Day15
  private val example =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin
  private lazy val puzzle: String = Resource("Day15.txt").content


  "Day15" should "solve example" in {
    sut.solve(example, 10, 20) shouldBe Solution(26, 56000011)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle, 2000000, 4000000) shouldBe Solution(4737443, 11482462818989L)
  }

  it should "parse lines" in {
    val first = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    val second = "Sensor at x=8, y=7: closest beacon is at x=2, y=10"

    sut.parseSensor(first) shouldBe Sensor(Point(2, 18), Point(-2, 15), 7)
    sut.parseSensor(second) shouldBe Sensor(Point(8, 7), Point(2, 10), 9)
  }

  "Sensor" should "give area cut at point" in {
    val sensor = Sensor(Point(8, 7), Point(2, 10), 9)

    sensor.lineCutAtWithoutBeacon(10) shouldBe Some(Line(Point(3, 10), Point(14, 10)))
  }
}
