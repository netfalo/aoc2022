import scala.annotation.tailrec

object Day20 extends Problem[Int, Int] {
  override def solve(puzzle: String): Solution[Int, Int] = {
    val numbers = parse(puzzle)
    val firstPart = solveFirstPart(numbers)

    Solution(firstPart, 0)
  }

  def parse(sequence: String): Vector[Int] = {
    sequence
      .split('\n')
      .map(Integer.parseInt)
      .toVector
  }

  def solveFirstPart(numbers: Vector[Int]): Int = {
    val size = numbers.length

    @tailrec
    def rec(acc: Vector[Int], numbers: Vector[Int]): Vector[Int] = {
      println(acc)
      if (numbers.isEmpty) acc
      else {
        val currentIndex = acc.indexOf(numbers.head)
        val targetIndex =
          if ((currentIndex + numbers.head) % size < 0) (currentIndex + numbers.head + size) % size
          else (currentIndex + numbers.head) % size

        val (first, second) = acc.splitAt(currentIndex)
        val updated1 = first ++ second.tail
        val (f2, s2) = updated1.splitAt(targetIndex)
        val updated = f2 ++ Vector(numbers.head) ++ s2

        rec(updated, numbers.tail)
      }

    }

    val mixed = rec(numbers, numbers)
    Seq(1000, 2000, 3000)
      .map(x => mixed(x % size))
      .sum
  }
}
