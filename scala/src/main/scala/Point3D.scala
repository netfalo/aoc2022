case class Point3D(x: Int, y: Int, z: Int) {
  def +(that: Point3D): Point3D = {
    Point3D(x + that.x, y + that.y, z + that.z)
  }

  def isAdjacentTo(that: Point3D): Boolean = {
    Math.abs(x - that.x) == 1 && y == that.y && z == that.z ||
      Math.abs(y - that.y) == 1 && x == that.x && z == that.z ||
      Math.abs(z - that.z) == 1 && y == that.y && x == that.x
  }

  def neighbours(): Set[Point3D] =
    Set(
      Point3D(-1, 0, 0),
      Point3D(1, 0, 0),
      Point3D(0, -1, 0),
      Point3D(0, 1, 0),
      Point3D(0, 0, -1),
      Point3D(0, 0, 1)
    )
      .map(_ + this)

  def cubeNeighbours(): Set[Point3D] =
    (for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      if x != 0 || y != 0 || z != 0
    } yield Point3D(x, y, z))
      .toSet
      .map((p: Point3D) => p + this)

}
