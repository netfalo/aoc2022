import scala.annotation.tailrec

object Day18 extends Problem[Int, Int] {

  case class Cavity(cubes: Set[Point3D]) {
    def isAdjacentTo(that: Cavity): Boolean =
      that.cubes.exists(p => cubes.exists(_.isAdjacentTo(p)))

    def +(that: Cavity): Cavity = {
      require(isAdjacentTo(that))

      Cavity(cubes ++ that.cubes)
    }

    def neighbours(): Set[Point3D] =
      cubes
        .flatMap(_.neighbours())
        .diff(cubes)
  }

  override def solve(puzzle: String): Solution[Int, Int] = {
    val cubes = parseCubes(puzzle)
    val firstPart = solveFirstPart(cubes)
    val secondPart = solveSecondPart(cubes)

    Solution(firstPart, secondPart)
  }

  private def solveFirstPart(cubes: Set[Point3D]): Int = {
    cubes
      .toSeq
      .map(p => 6 - p.neighbours().intersect(cubes).size)
      .sum
  }

  private def solveSecondPart(cubes: Set[Point3D]): Int = {
    val minx = cubes.minBy(_.x).x
    val miny = cubes.minBy(_.y).y
    val minz = cubes.minBy(_.z).z
    val maxx = cubes.maxBy(_.x).x
    val maxy = cubes.maxBy(_.y).y
    val maxz = cubes.maxBy(_.z).z
    val mn = Seq(minx, miny, minz).min
    val mx = Seq(maxx, maxy, maxz).max

    val min = Point3D(mn, mn, mn) + Point3D(-1, -1, -1)
    val max = Point3D(mx, mx, mx) + Point3D(1, 1, 1)

    flood(Set(min), min, max, cubes)
      .toList
      .flatMap(_.neighbours().intersect(cubes))
      .size
  }

  @tailrec
  def flood(floodedArea: Set[Point3D], min: Point3D, max: Point3D, cubes: Set[Point3D]): Set[Point3D] = {
    val neighbours = floodedArea
      .flatMap(_.neighbours() -- cubes)

    val expandedArea = (neighbours ++ floodedArea)
      .filter(p => p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y && p.z >= min.z && p.z <= max.z)

    if (floodedArea.size == expandedArea.size) floodedArea
    else flood(expandedArea, min, max, cubes)
  }

  def parseCubes(input: String): Set[Point3D] =
    input.split("\n")
      .toSet
      .map((x: String) => x.trim())
      .filterNot(_.isBlank)
      .map(_.split(",").map(Integer.parseInt))
      .map {
        case Array(x, y, z) => Point3D(x, y, z)
      }
}
