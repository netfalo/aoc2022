object Day2 extends Problem[Int, Int] {
  sealed trait Move {
    def value: Int
  }
  object Rock extends Move {
    override def value: Int = 1
  }

  object Paper extends Move {
    override def value: Int = 2
  }

  object Scissor extends Move {
    override def value: Int = 3
  }

  def parseMove(result: String): Move = result match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissor
  }

  sealed trait Result {
    def value: Int
  }
  object Loose extends Result {
    override def value: Int = 0
  }
  object Draw extends Result {
    override def value: Int = 3
  }

  object Win extends Result {
    override def value: Int = 6
  }

  def parseResult(result: String): Result = result match {
    case "X" => Loose
    case "Y" => Draw
    case "Z" => Win
  }

  case class GameState(me: Move, opponent: Move) {
    def evaluate: Result = (me, opponent) match {
      case (a, b) if a == b => Draw
      case (Rock, Paper) => Loose
      case (Paper, Rock) => Win
      case (Rock, Scissor) => Win
      case (Scissor, Rock) => Loose
      case (Paper, Scissor) => Loose
      case (Scissor, Paper) => Win
    }

    def score: Int = evaluate.value + me.value
  }

  override def solve(puzzle: String): Solution[Int, Int] = {
    val firstSolution = puzzle
      .split("\n")
      .map(_.trim())
      .filterNot(_.isEmpty())
      .map(parseGame)
      .map(_.score)
      .sum

    val secondSolution = puzzle
      .split("\n")
      .map(_.trim())
      .filterNot(_.isEmpty())
      .map(parseInstruction)
      .map {
        case (opponentsMove, expectedResult) =>
          GameState(suggestMove(opponentsMove, expectedResult), opponentsMove)
      }
      .map(_.score)
      .sum

    Solution(firstSolution, secondSolution)
  }

  def parseGame(game: String): GameState = game match {
    case s"$opponent $me" => GameState(me match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissor
    }, parseMove(opponent))
  }

  def parseInstruction(line: String): (Move, Result) = line match {
    case s"$move $result" => (parseMove(move), parseResult(result))
  }

  def suggestMove(move: Move, expectedResult: Result): Move = (move, expectedResult) match {
    case (x, Draw) => x
    case (Rock, Win) => Paper
    case (Paper, Win) => Scissor
    case (Scissor, Win) => Rock
    case (Rock, Loose) => Scissor
    case (Paper, Loose) => Rock
    case (Scissor, Loose) => Paper
  }
}
