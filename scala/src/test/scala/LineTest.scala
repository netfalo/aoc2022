class LineTest extends AnyFlatSpec {
  "Line" should "merge" in {
    Line(Point(1, 3), Point(4, 3)) + Line(Point(2, 3), Point(5, 3)) shouldBe Line(Point(1,3), Point(5,3))
    Line(Point(4, 3), Point(1, 3)) + Line(Point(2, 3), Point(5, 3)) shouldBe Line(Point(1,3), Point(5,3))
    Line(Point(2, 3), Point(5, 3)) + Line(Point(4, 3), Point(1, 3)) shouldBe Line(Point(1,3), Point(5,3))
    Line(Point(1, 3), Point(5, 3)) + Line(Point(2, 3), Point(4, 3)) shouldBe Line(Point(1,3), Point(5,3))
    Line(Point(20,10),Point(8,10)) +  Line(Point(22,10),Point(18,10)) shouldBe Line(Point(8,10), Point(22,10))
  }
}
