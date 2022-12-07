import scala.annotation.tailrec

object Day7 extends Problem[Long, Long] {
  sealed trait Item {
    def size(): Long
  }

  case class File(name: String, fileSize: Long) extends Item {
    override def size(): Long = fileSize
  }

  case class Directory(name: String, content: Seq[Item]) extends Item {
    override def size(): Long = content
      .foldLeft(0L) {
        case (acc, item) => acc + item.size()
      }
  }

  override def solve(puzzle: String): Solution[Long, Long] = {
    val fileSystem = buildFileTree(parseLines(puzzle))

    Solution(solveFirstPart(fileSystem), solveSecondPart(fileSystem))
  }

  def solveFirstPart(directory: Directory): Long = {
    findDirectories(directory).foldLeft(0L) {
      case (acc, item) => acc + item.size()
    }
  }

  def findDirectories(directory: Directory): Seq[Directory] = {
    val current = if (directory.size() < 100000)
      Seq(directory)
    else Seq()

    directory.content
      .filter {
        case m: File => false
        case m: Directory => true
      }
      .flatMap {
        case m: Directory => findDirectories(m)
      } ++ current
  }

  def solveSecondPart(directory: Directory): Long = {
    val TOTAL_SPACE = 70000000L
    val REQUIRED_FREE_SPACE = 30000000L - (TOTAL_SPACE - directory.size())

    findSmallestAbove(directory, REQUIRED_FREE_SPACE, directory).size()
  }

  def findSmallestAbove(directory: Directory, requiredPlace: Long, currentMin: Directory): Directory = {
    val x = directory.content
      .filter {
        case m: File => false
        case m: Directory => true
      }
      .map {
        case m: Directory => findSmallestAbove(m, requiredPlace, if (currentMin.size() > directory.size() && directory.size() > requiredPlace) directory else currentMin)
      }

    if (x.nonEmpty && x.minBy(_.size()).size() < currentMin.size())
      x.minBy(_.size())
    else if (currentMin.size() > directory.size() && directory.size() > requiredPlace)
      directory
    else
      currentMin
  }


  def parseLines(input: String): Seq[Seq[String]] = {
    input
      .split("\\$")
      .map(_.split("\n").map(_.trim()).filterNot(_.isBlank()).toVector)
      .filterNot(_.isEmpty)
      .toVector
  }

  def buildFileTree(commands: Seq[Seq[String]]): Directory = {
    @tailrec
    def buildFileTree(currentPath: Seq[String], tree: Map[String, Seq[String]], files: Map[String, Seq[File]], commands: Seq[Seq[String]]): (Map[String, Seq[String]], Map[String, Seq[File]]) = {
      if (commands.isEmpty)
        (tree, files)
      else
        commands.head match {
          case m if m.head == "ls" =>
            val updatedTree = tree
              .updated(currentPath.mkString("/"), m.tail.filter(_.startsWith("dir ")).map(_.split(" ")(1)).map(currentPath.appended(_).mkString("/")))
            val updatedFiles =
              if (m.tail.exists(!_.startsWith("dir ")))
                files
                  .updated(currentPath.mkString("/"), m.tail.filter(!_.startsWith("dir ")).map { case s"$size $name" => File(name, size.toLong)})
              else
                files
            buildFileTree(currentPath, updatedTree, updatedFiles, commands.tail)
          case m if m.head == "cd .." =>
            buildFileTree(currentPath.reverse.tail.reverse, tree, files, commands.tail)
          case m if m.head.startsWith("cd") =>
            val newPath = currentPath.appended(m.head.split(" ")(1))
            buildFileTree(newPath, tree, files, commands.tail)
        }
    }

    val (tree, files) = buildFileTree(Seq(), Map(), Map(), commands.tail)

    def buildDirectory(name: String): Directory = {
      if (files.getOrElse(name, Seq()).map(_.name) == tree(name)) {
        Directory(name, files(name))
      } else {
        Directory(name, tree(name)
          .map(buildDirectory) ++ files.getOrElse(name, Seq()))
      }
    }

    buildDirectory("")
  }
}
