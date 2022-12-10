import scala.annotation.tailrec

object Day10 extends Problem[Int, String] {
  sealed trait Instruction

  case class Addx(value: Int) extends Instruction

  case object Noop extends Instruction

  override def solve(puzzle: String): Solution[Int, String] = {
    val program = parseProgram(puzzle)
    val programOutput = runProgram(program)
    val firstSolution: Int = calculateSolutionForFirstPart(programOutput)
    val secondSolution = parseImage(renderImage(programOutput))

    Solution(firstSolution, secondSolution)
  }

  def calculateSolutionForFirstPart(programOutput: Vector[(Int, Int)]): Int = {
    programOutput
      .filter {
        case (index, _) => index >= 19 && ((index - 19) % 40) == 0
      }
      .map { case (cycle, value) => value * (cycle + 1) }
      .sum
  }

  def decodeCharacter(image: String): (Double, Char) = {
    val E = "####\n#...\n###.\n#...\n#...\n####"
    val K = "#..#\n#.#.\n##..\n#.#.\n#.#.\n#..#"
    val R = "###.\n#..#\n#..#\n###.\n#.#.\n#..#"
    val H = "#..#\n#..#\n####\n#..#\n#..#\n#..#"
    val P = "###.\n#..#\n#..#\n###.\n#...\n#..."
    val U = "#..#\n#..#\n#..#\n#..#\n#..#\n.##."
    val Z = "####\n...#\n..#.\n.#..\n#...\n####"
    val capitalLetters = Vector((E, 'E'), (K, 'K'), (R, 'R'), (H, 'H'), (P, 'P'), (U, 'U'), (Z, 'Z'))

    capitalLetters
      .find {
        case (img, _) => image == img
      }
      .map { case (_, chr) => (1.0, chr) }
      .getOrElse(capitalLetters.map { case (img, chr) => (img.zip(image).count { case (expected, actual) => expected == actual } / 29.0, chr) }.maxBy(_._1))
  }

  def parseImage(image: Vector[String]): String = {
    image
      .map(_.grouped(5).toVector)
      .foldLeft(Vector.fill(8)(Vector[String]())) {
        case (acc, item) => acc.zip(item).map { case (x, y) => x.appended(y) }
      }
      .map(x => x.map(_.take(4).mkString).mkString("\n"))
      .foldLeft("") {
        case (acc, img) =>
          val (confidence, char) = decodeCharacter(img)
          if (confidence == 1.0) acc + char
          else {
            println(s"$img cannot be decoded with 100% confidence, best guess '$char' with ${confidence * 100} confidence")
            acc + ' '
          }
      }
  }

  def parseProgram(input: String): Vector[Instruction] = {
    input
      .split("\n")
      .foldLeft(Vector[Instruction]()) {
        case (acc, current) if current.isBlank => acc
        case (acc, current) => current.trim match {
          case "noop" => acc.appended(Noop)
          case s"addx $value" => acc.appended(Addx(Integer.parseInt(value)))
        }
      }
  }

  def runProgram(program: Vector[Instruction]): Vector[(Int, Int)] = {
    @tailrec
    def runProgram(program: Vector[Instruction], register: Int, cycle: Int, acc: Vector[Int]): Vector[Int] = {
      if (program.isEmpty) acc
      else {
        program.head match {
          case Noop =>
            runProgram(program.tail, register, cycle + 1, acc.appended(register))
          case Addx(value) =>
            runProgram(program.tail, register + value, cycle + 2, acc.appended(register).appended(register))
        }
      }
    }

    runProgram(program, 1, 1, Vector())
      .zipWithIndex.map { case (value, index) => (index, value) }
  }

  def renderImage(registers: Vector[(Int, Int)]): Vector[String] = {
    def renderImage(registers: Vector[(Int, Int)], acc: Vector[Char]): Vector[Char] = {
      if (registers.isEmpty) acc
      else {
        val (index, value) = registers.head
        if (value - 1 <= (index % 40) && value + 1 >= (index % 40))
          renderImage(registers.tail, acc.appended('#'))
        else
          renderImage(registers.tail, acc.appended('.'))
      }
    }

    val result = renderImage(registers, Vector())

    result
      .grouped(40)
      .map(_.mkString)
      .toVector
  }
}
