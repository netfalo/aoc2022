import scala.collection.immutable.Queue
object Day5 extends Problem[String, String] {
  override def solve(puzzle: String): Solution[String, String] = {
    val lines = parseLines(puzzle)

    Solution(solveFirstPart(lines._1, lines._2), solveSecondPart(lines._1, lines._2))
  }

  def solveFirstPart(stacks: Vector[Vector[Char]], instructions: Seq[(Int, Int, Int)]): String = {
    if (instructions.isEmpty)
      stacks.flatMap(_.headOption).mkString
    else {
      val (nr, from, to) = instructions.head
      val toBeMoved = stacks(from).take(nr).reverse
      val newStacks = stacks.zipWithIndex
        .map {
          case (stack, i) if i == from => stack.drop(nr)
          case (stack, i) if i == to => stack.prependedAll(toBeMoved)
          case (stack, _) => stack
        }
        .toVector

      solveFirstPart(newStacks, instructions.tail)
    }
  }

  def solveSecondPart(stacks: Vector[Vector[Char]], instructions: Seq[(Int, Int, Int)]): String = {
    if (instructions.isEmpty)
      stacks.flatMap(_.headOption).mkString
    else {
      val (nr, from, to) = instructions.head
      val toBeMoved = stacks(from).take(nr)
      val newStacks = (0 until stacks.size)
        .map {
          case i if i == from => stacks(i).drop(nr)
          case i if i == to => stacks(i).prependedAll(toBeMoved)
          case i => stacks(i)
        }
        .toVector

      solveSecondPart(newStacks, instructions.tail)
    }
  }

  def parseLines(input: String): (Vector[Vector[Char]], Seq[(Int, Int, Int)]) = {
    val Array(stacks, instructions) = input.split("\n\n")

    (parseStacks(stacks), parseInstructions(instructions))
  }

  def parseStacks(lines: String): Vector[Vector[Char]] = {
    def parseStacks(acc: Vector[Vector[Char]], lines: Array[String]): Vector[Vector[Char]] = {
      if (lines.forall(_.isBlank())) acc
      else {
        val stack = lines.flatMap(_.take(3)).filter(_.isLetter).toVector
        val remainingLines = lines.map(_.drop(4))

        parseStacks(acc.appended(stack), remainingLines)
      }
    }

    val cleaned = lines
      .split("\n")
      .filterNot(_.isBlank())
    val numberOfStacks = cleaned.reverse.head.count(_.isDigit)

    parseStacks(Vector(), cleaned)
  }

  def parseInstructions(lines: String): Seq[(Int, Int, Int)] = {
    lines
      .split("\n")
      .filterNot(_.isBlank())
      .map {
        case s"move $nr from $from to $to" => (nr.toInt, from.toInt - 1, to.toInt - 1)
      }
      .toSeq
  }
}
