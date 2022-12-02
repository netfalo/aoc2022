import org.scalatest.matchers.should.Matchers
import scala.io.Source
import Day2._


class Day2Test extends AnyFlatSpec {
  private val sut = Day2
  private val example = """
A Y
B X
C Z
"""
  private lazy val puzzle: String = Resource("Day2.txt").content

  "Day2" should "solve example" in {
    sut.solve(example) shouldBe Solution(15, 12)
  }

  "Day2" should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(10595, 9541)
  }

  "parseGame" should "return tuple" in {
    sut.parseGame("A Y") shouldBe GameState(Paper, Rock)
    sut.parseGame("B X") shouldBe GameState(Rock, Paper)
    sut.parseGame("C Z") shouldBe GameState(Scissor, Scissor)
  }

  "parseInstruction" should "return tuple" in {
    sut.parseInstruction("A Y") shouldBe (Rock, Draw)
    sut.parseInstruction("B X") shouldBe (Paper, Loose)
    sut.parseInstruction("C Z") shouldBe (Scissor, Win)
  }

  "GameState" should "give score" in {
    GameState(Paper, Rock).score shouldBe 8
    GameState(Rock, Paper).score shouldBe 1
    GameState(Rock, Scissor).score shouldBe 7
    GameState(Scissor, Rock).score shouldBe 3
    GameState(Paper, Scissor).score shouldBe 2
    GameState(Scissor, Paper).score shouldBe 9
    GameState(Rock, Rock).score shouldBe 4
    GameState(Paper, Paper).score shouldBe 5
    GameState(Scissor, Scissor).score shouldBe 6
  }

  it should "tell the Result" in {
    GameState(Paper, Rock).evaluate shouldBe Win
    GameState(Rock, Paper).evaluate shouldBe Loose
    GameState(Rock, Scissor).evaluate shouldBe Win
    GameState(Scissor, Rock).evaluate shouldBe Loose
    GameState(Scissor, Paper).evaluate shouldBe Win
    GameState(Paper, Scissor).evaluate shouldBe Loose
    GameState(Rock, Rock).evaluate shouldBe Draw
    GameState(Paper, Paper).evaluate shouldBe Draw
    GameState(Scissor, Scissor).evaluate shouldBe Draw
  }

}
