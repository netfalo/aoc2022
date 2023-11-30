case class Point(x: Int, y: Int) {
  def -(that: Point): Point = Point(x - that.x, y - that.y)

  def +(that: Point): Point = Point(x + that.x, y + that.y)

  def manhattanDistanceTo(point: Point): Int = Math.abs(x - point.x) + Math.abs(y - point.y)
}
