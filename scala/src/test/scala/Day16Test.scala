class Day16Test extends AnyFlatSpec {
  private val sut = Day16
  private val example =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

  private lazy val puzzle: String = Resource("Day16.txt").content


  "Day16" should "solve example" in {
    sut.solve(example) shouldBe Solution(1651, 1707)
  }

  it should "solve puzzle" in {
    sut.solve(puzzle) shouldBe Solution(1880, 2520)
  }

  it should "parse valves" in {
    sut.parseValves(example) shouldBe(
      Map("AA" -> 0, "BB" -> 13, "CC" -> 2, "DD" -> 20, "EE" -> 3, "FF" -> 0, "GG" -> 0, "HH" -> 22, "II" -> 0, "JJ" -> 21),
      Map(
        "AA" -> Set("DD", "II", "BB"),
        "BB" -> Set("CC", "AA"),
        "CC" -> Set("DD", "BB"),
        "DD" -> Set("CC", "AA", "EE"),
        "EE" -> Set("FF", "DD"),
        "FF" -> Set("EE", "GG"),
        "GG" -> Set("FF", "HH"),
        "HH" -> Set("GG"),
        "II" -> Set("AA", "JJ"),
        "JJ" -> Set("II")
      )
    )
  }

  it should "calculate distance between all nodes" in {
    val connections = Map(
      "AA" -> Set("DD", "II", "BB"),
      "BB" -> Set("CC", "AA"),
      "CC" -> Set("DD", "BB"),
      "DD" -> Set("CC", "AA", "EE"),
      "EE" -> Set("FF", "DD"),
      "FF" -> Set("EE", "GG"),
      "GG" -> Set("FF", "HH"),
      "HH" -> Set("GG"),
      "II" -> Set("AA", "JJ"),
      "JJ" -> Set("II")
    )

    sut.dijkstra("AA", connections) shouldBe Map(
      "JJ" -> "II",
      "II" -> "AA",
      "BB" -> "AA",
      "DD" -> "AA",
      "CC" -> "BB",
      "EE" -> "DD",
      "FF" -> "EE",
      "GG" -> "FF",
      "HH" -> "GG",
    )
  }

  it should "reconstruct path correctly" in {
    val previousSteps = Map(
      "JJ" -> "II",
      "II" -> "AA",
      "BB" -> "AA",
      "DD" -> "AA",
      "CC" -> "BB",
      "EE" -> "DD",
      "FF" -> "EE",
      "GG" -> "FF",
      "HH" -> "GG",
    )

    sut.rebuildPath("AA", "HH", Seq(), previousSteps) shouldBe Seq("DD", "EE", "FF", "GG", "HH")
  }

}
