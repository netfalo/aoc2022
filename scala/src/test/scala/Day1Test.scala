import org.scalatest.matchers.should.Matchers
import scala.io.Source


class Day1Test extends AnyFlatSpec {
  private val sut = new Day1
  private val example = """
    1000
    2000
    3000

    4000

    5000
    6000

    7000
    8000
    9000

    10000"""

  private lazy val puzzle: String = Resource("Day1.txt").content

  "Day1" should "solve example" in {
    sut.solve(example) shouldBe Solution(24000, 45000)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(68775, 202585)
  }
}
