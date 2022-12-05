class Day5Test extends AnyFlatSpec {
  private val sut = Day5
  private val example = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

"""

  private lazy val puzzle: String = Resource("Day5.txt").content

  "Day5" should "solve example" in {
    Day5.solve(example) shouldBe Solution("CMZ", "MCD")
  }

  it should "solve puzzle" in {
    Day5.solve(puzzle) shouldBe Solution("FRDSQRRCD", "HRFTQVWNN")
  }

  it should "parse the stacks from the example" in {
    val stacks = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 """
    Day5.parseStacks(stacks) shouldBe Vector(Vector('N', 'Z'), Vector('D', 'C', 'M'), Vector('P'))
  }

  it should "parse the instructions from the example" in {
    val instructions = """
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

"""

    Day5.parseInstructions(instructions) shouldBe Vector((1,1,0), (3,0,2), (2,1,0), (1,0,1))
  }

  it should "move one" in {
    Day5.solveFirstPart(Vector(Vector('N', 'Z'), Vector('D', 'C', 'M'), Vector('P')), Vector((1, 1, 0))) shouldBe "DCP"
  }

  it should "move 3" in {
    Day5.solveFirstPart(Vector(Vector('D', 'N', 'Z'), Vector('C', 'M'), Vector('P')), Vector((3, 0, 2))) shouldBe "CZ"
  }
}
