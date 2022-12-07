import Day7.{Directory, File}

class Day7Test extends AnyFlatSpec {
  private val sut = Day7
  private val example =
    """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

  private lazy val puzzle: String = Resource("Day7.txt").content

  "Day7" should "solve example" in {
    sut.solve(example) shouldBe Solution(95437L, 24933642L)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(1667443L, 8998590L)
  }

  it should "parse file system" in {
    val tree = Directory("",
      Vector(
        Directory("a",
          Vector(
            Directory("a/e",
              Vector(
                File("i", 584)
              )
            ),
            File("f", 29116),
            File("g", 2557),
            File("h.lst", 62596)
          )
        ),
        Directory("d",
          Vector(
            File("j", 4060174),
            File("d.log", 8033020),
            File("d.ext", 5626152),
            File("k", 7214296)
          )
        ),
        File("b.txt", 14848514),
        File("c.dat", 8504156)
      )
    )
    sut.buildFileTree(sut.parseLines(example)) shouldBe tree
  }
}
