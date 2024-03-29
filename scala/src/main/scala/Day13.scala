import scala.annotation.tailrec

object Day13 extends Problem[Int, Int] {

  val dividers: Seq[String] = Seq("[[2]]", "[[6]]")

  override def solve(puzzle: String): Solution[Int, Int] = {
    val packetPairs = parseLines(puzzle)

    val firstPart = solveFirstPart(packetPairs)
      .filter(_._2 == -1)
      .map(_._1)
      .sum

    val packetsWithDividers =
      sortPacketsWithDividers(packetPairs
        .flatMap { case (l, r) => Seq(l, r) }
      )

    val secondPart =
      dividers
        .map(packetsWithDividers.indexOf(_) + 1)
        .foldLeft(1) { case (acc, item) => acc * item }

    Solution(firstPart, secondPart)
  }

  def sortPacketsWithDividers(packets: Seq[String]): Seq[String] =
    packets
      .appendedAll(dividers)
      .sortWith { case (l, r) => comparePackets(l, r) == -1 }

  def solveFirstPart(packetPairs: Vector[(String, String)]): Vector[(Int, Int)] = {
    val solution = packetPairs
      .map { case (left, right) => comparePackets(left, right) }
      .zipWithIndex
      .map {
        case (result, index) => (index + 1, result)
      }

    solution
  }

  def parseLines(input: String): Vector[(String, String)] =
    input
      .split("\n\n")
      .filterNot(_.isBlank)
      .map(_.split("\n").filterNot(_.isBlank))
      .map {
        case Array(left, right) => (left, right)
      }
      .toVector


  def isNumber(str: String): Boolean = !str.isBlank && str.forall(_.isDigit)

  def isList(str: String): Boolean = str.startsWith("[") && str.endsWith("]")

  def comparePackets(left: String, right: String): Int = {
    if (left == "[]" && right != "[]")
      return -1
    else if (left != "[]" && right == "[]") return 1
    else if (left == "[]" && right == "[]") return 0

    val (leftHead, leftTail) = findHeadAndTail(left)
    val (rightHead, rightTail) = findHeadAndTail(right)

    val comparisonResult =
      if (isNumber(leftHead) && isNumber(rightHead)) {
        Integer.compare(Integer.parseInt(leftHead), Integer.parseInt(rightHead))
      } else if (isList(leftHead) && isList(rightHead)) {
        comparePackets(leftHead, rightHead)
      } else if (isNumber(leftHead) && !isNumber(rightHead)) {
        comparePackets(s"[$leftHead]", rightHead)
      } else if (!isNumber(leftHead) && isNumber(rightHead)) {
        comparePackets(leftHead, s"[$rightHead]")
      } else {
        throw new RuntimeException(s"How did this happen lh: '$leftHead', lt: '$leftTail', rh: '$rightHead', rt: '$rightTail'")
      }

    comparisonResult match {
      case -1 => -1
      case 0 => comparePackets(leftTail, rightTail)
      case 1 => 1
    }
  }

  def findHeadAndTail(str: String): (String, String) = {
    if (str.isBlank) return ("", "")
    val (h, t, _, _, _) = str.substring(1, str.length - 1)
      .foldLeft(("", "", 0, 0, false)) {
        case ((head, tail, 0, 0, false), c) if c.isDigit => (head + c, tail, 0, 0, false)
        case ((head, tail, 0, 0, false), ',') => (head, tail, 0, 0, true)
        case ((head, tail, leftBracket, rightBracket, false), '[') => (head + "[", tail, leftBracket + 1, rightBracket, false)
        case ((head, tail, leftBracket, rightBracket, false), ']') =>
          if (leftBracket - 1 == rightBracket) (head + "]", tail, leftBracket, rightBracket + 1, true)
          else (head + "]", tail, leftBracket, rightBracket + 1, false)
        case ((head, tail, leftBracket, rightBracket, false), x) => (head + x, tail, leftBracket, rightBracket, false)
        case ((head, tail, leftBracket, rightBracket, true), x) => (head, tail + x, leftBracket, rightBracket, true)
      }

    if (t.isBlank)
      (h, "[]")
    else
      (
        if (h.startsWith(",")) h.substring(1) else h,
        if (t.startsWith(",")) "[" + t.substring(1) + "]" else "[" + t + "]"
      )
  }

  sealed trait Tree

  case class Leaf(values: Int*) extends Tree

  case object Empty extends Tree

  case class Node(values: Tree*) extends Tree

  object TreeOrdering extends Ordering[Tree] {
    override def compare(x: Tree, y: Tree): Int = (x, y) match {
      case (Empty, Empty) => 0
      case (Empty, _) => -1
      case (_, Empty) => 1
      case (left: Leaf, right: Leaf) =>
        val allEquals = left.values.zip(right.values)
          .forall { case (x, y) => x == y }
        if (allEquals && left.values.size < right.values.size) -1
        else if (allEquals && left.values.size > right.values.size) 1
        else if (allEquals && left.values.size == right.values.size) 0
        else if (left.values.zip(right.values).forall { case (x, y) => x <= y }) -1
        else 1
      case (left: Node, right: Leaf) => compare(left, Node(right))
      case (left: Leaf, right: Node) => compare(Node(left), right)
      case (left: Node, right: Node) =>
        val allEquals = left.values.zip(right.values).forall { case (l, r) => compare(l, r) == 0 }
        if (allEquals && left.values.size < right.values.size) -1
        else if (allEquals && left.values.size > right.values.size) 1
        else if (allEquals && left.values.size == right.values.size) 0
        else {
          val paired = left.values.zip(right.values)
          val firstLeftBigger = paired.indexWhere { case (l, r) => compare(l, r) == 1 }
          val firstRightBigger = paired.indexWhere { case (l, r) => compare(l, r) == -1 }

          if (firstRightBigger == -1) -1
          else if (firstRightBigger > firstLeftBigger) -1
          else 1
        }
    }
  }

  def parseTree(input: String): Tree = {
    if (input == "[]") Empty
    else if (isNumber(input)) Leaf(Integer.parseInt(input))
    else if (isList(input) && !input.substring(1, input.length - 1).exists(c => '[' == c || c == ']')) {
      val numbers = input.substring(1, input.length - 1).split(",").map(Integer.parseInt)
      Leaf(numbers: _*)
    } else if (isList(input)) {
      val nodes = splitList(input)
        .map(parseTree)

      Node(nodes: _*)
    } else {
      throw new RuntimeException("This is strange")
    }
  }

  def splitList(str: String): Seq[String] = {
    require(isList(str), "It must be always a list")
    val unwrapped = str.substring(1, str.length - 1)

    val (items, lastItem, leftBracket, rightBracket) = unwrapped
      .foldLeft((Seq[String](), "", 0, 0)) {
        case ((acc, current, leftBracket, rightBracket), ',') if leftBracket == rightBracket =>
          (acc.appended(current), "", leftBracket, rightBracket)
        case ((acc, current, leftBracket, rightBracket), '[') =>
          (acc, current + "[", leftBracket + 1, rightBracket)
        case ((acc, current, leftBracket, rightBracket), ']') =>
          (acc, current + "]", leftBracket, rightBracket + 1)
        case ((acc, current, leftBracket, rightBracket), x) =>
          (acc, current + x, leftBracket, rightBracket)
      }

    require(leftBracket == rightBracket, s"'$items' '$lastItem'")
    items
      .appended(lastItem)
  }
}
