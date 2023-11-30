//This class assumes every line is vertical
case class Line(start: Point, end: Point) {
  require(start.x <= end.x)

  def contains(point: Point): Boolean =
    start.x <= point.x && point.x <= end.x
  //Assumes every line is parallel
  def isTouching(other: Line): Boolean = {
    other.start.y == start.y &&
      (other.start.x <= start.x && start.x <= other.end.x ||
        start.x <= other.start.x && other.start.x <= end.x ||
        end.x + 1 == other.start.x ||
        other.end.x + 1 == start.x)
  }

  def +(other: Line): Line = {
    val points = Seq(start, end, other.start, other.end)
    Line(points.minBy(_.x), points.maxBy(_.x))
  }

  lazy val length: Int = Math.abs(start.x - end.x) + 1

  def intersect(line: Line): Line = {
    require(line.start.y == start.y)
    Line(Point(Math.max(line.start.x, start.x), start.y), Point(Math.min(line.end.x, end.x), start.y))
  }
}
