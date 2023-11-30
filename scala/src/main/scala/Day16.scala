import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableSetIsParallelizable

object Day16 extends Problem[Int, Int] {

  override def solve(puzzle: String): Solution[Int, Int] = {
    val (flowRates, connections) = parseValves(puzzle)
    val previousStep = distanceBetweenAllNodes(connections, flowRates)
    val totalReleasedPressureAlone = openValvesToMaximizeFlowRateUnderGivenTime(flowRates, previousStep, 30)
    val totalReleasedPressureWithAnElephant = openValvesToMaximizeFlowRateUnderGivenTimeWithAnElephant(flowRates, previousStep, 26)

    Solution(totalReleasedPressureAlone, totalReleasedPressureWithAnElephant)
  }

  def parseValve(line: String): (String, Int, Set[String]) = line match {
    case s"Valve $valve has flow rate=$flowRate; tunnels lead to valves $connectedValves" =>
      (valve, Integer.parseInt(flowRate), connectedValves.split(", ").toSet)
    case s"Valve $valve has flow rate=$flowRate; tunnel leads to valve $connectedValve" =>
      (valve, Integer.parseInt(flowRate), Set(connectedValve))
  }

  def parseValves(str: String): (Map[String, Int], Map[String, Set[String]]) = {
    LazyList
      .from(str.split("\n"))
      .filterNot(_.isBlank)
      .map(_.trim)
      .map(line => parseValve(line))
      .foldLeft((Map[String, Int](), Map[String, Set[String]]())) {
        case ((flowRates, connections), (valve, flowRate, connectedValves)) =>
          (flowRates.updated(valve, flowRate), connections.updated(valve, connectedValves))
      }
  }

  def distanceBetweenAllNodes(connections: Map[String, Set[String]], flowRates: Map[String, Int]): Map[String, Map[String, Int]] =
    connections
      .keys
      .map(n => (n, dijkstra(n, connections)))
      .foldLeft(Map[String, Map[String, Int]]()) {
        case (p, (n, cp)) if flowRates(n) != 0 || n == "AA" => p.updated(n, cp)
        case (p, (n, _)) if flowRates(n) == 0 => p
      }

  def dijkstra(source: String, connections: Map[String, Set[String]]): Map[String, Int] = {
    @tailrec
    def dijkstra(distances: Map[String, Int], previous: Map[String, String], toVisit: Map[String, Int]): Map[String, Int] = {
      if (toVisit.isEmpty) distances
      else {
        val current = toVisit.minBy(_._2)._1
        val neighbours = connections(current)
        val (updatedDistances, updatedPrevs) =
          neighbours.foldLeft((distances, previous)) {
            case ((dst, prv), c) =>
              if (!dst.contains(c) || dst(c) > dst(current) + 1)
                (dst.updated(c, dst(current) + 1), prv.updated(c, current))
              else (dst, prv)
          }

        dijkstra(updatedDistances, updatedPrevs, toVisit.removed(current).map { case (n, d) => (n, updatedDistances(n)) })
      }
    }

    val initialDistances = connections.keys.map((_, Int.MaxValue)).toMap.updated(source, 0)
    dijkstra(initialDistances, Map(), initialDistances)
  }

  @tailrec
  def rebuildPath(source: String, end: String, acc: Seq[String], previousStep: Map[String, String]): Seq[String] = {
    if (source == end) acc
    else rebuildPath(source, previousStep(end), acc.prepended(end), previousStep)
  }

  def openValvesToMaximizeFlowRateUnderGivenTime(flowRates: Map[String, Int], distances: Map[String, Map[String, Int]], timeLimit: Int): Int = {
    def rec(currentPosition: String, currentTime: Int, toOpen: Set[String], valveOpenedAt: Map[String, Int]): Option[(Int, Map[String, Int])] = {
      toOpen
        .flatMap { candidate: String =>
          val distance = distances(currentPosition)(candidate)
          val arrivalTime = currentTime + distance + 1

          if (arrivalTime < timeLimit) {
            val updatedOpenedAt =
              valveOpenedAt.updated(candidate, currentTime + distance + 2)

            rec(candidate,
              arrivalTime,
              toOpen - candidate,
              updatedOpenedAt)
          }
          else None
        }
        .maxByOption(_._1)
        .orElse(Some((calculateTotalReleasedPressure(timeLimit, valveOpenedAt, flowRates), valveOpenedAt)))
    }

    val toOpen = flowRates.filter(_._2 > 0).keySet
    val Some((totalReleasedPressure, _)) = rec("AA", 0, toOpen, Map())

    totalReleasedPressure
  }

  def calculateTotalReleasedPressure(timeLimit: Int, valveOpenedAt: Map[String, Int], flowRates: Map[String, Int]): Int =
    valveOpenedAt
      .foldLeft(0) {
        case (total, (valve, timestamp)) => total + (timeLimit - timestamp + 1) * flowRates(valve)
      }

  def openValvesToMaximizeFlowRateUnderGivenTimeWithAnElephant(flowRates: Map[String, Int], distances: Map[String, Map[String, Int]], timeLimit: Int): Int = {
    def rec(currentPosition: (String, String), currentTime: (Int, Int), toOpen: Set[String], valveOpenedAt: Map[String, Int]): Option[(Int, Map[String, Int])] = {
      val (myPosition, elephantsPosition) = currentPosition
      val (myTime, elephantsTime) = currentTime
      generateCandidates(toOpen, currentTime, currentPosition, timeLimit, distances)
        .flatMap {
          case (myCandidate, elephantsCandidate) =>
            val distanceForMe = distances(myPosition)(myCandidate)
            val distanceForTheElephant = distances(elephantsPosition)(elephantsCandidate)
            val arrivalTimeForMe = myTime + distanceForMe + 1
            val arrivalTimeForTheElephant = elephantsTime + distanceForTheElephant + 1

            if (arrivalTimeForMe < timeLimit && arrivalTimeForTheElephant < timeLimit)
              rec((myCandidate, elephantsCandidate),
                (arrivalTimeForMe, arrivalTimeForTheElephant),
                toOpen - myCandidate - elephantsCandidate,
                valveOpenedAt
                  .updated(myCandidate, arrivalTimeForMe + 1)
                  .updated(elephantsCandidate, arrivalTimeForTheElephant + 1))

            else if (arrivalTimeForMe < timeLimit)
              rec((myCandidate, elephantsPosition),
                (arrivalTimeForMe, elephantsTime),
                toOpen - myCandidate,
                valveOpenedAt.updated(myCandidate, arrivalTimeForMe + 1))

            else if (arrivalTimeForTheElephant < timeLimit)
              rec((myPosition, elephantsCandidate),
                (myTime, arrivalTimeForTheElephant),
                toOpen - elephantsCandidate,
                valveOpenedAt.updated(elephantsCandidate, arrivalTimeForTheElephant + 1))
            else None
        }
        .maxByOption(_._1)
        .orElse(Some((calculateTotalReleasedPressure(timeLimit, valveOpenedAt, flowRates), valveOpenedAt)))
    }

    val toOpen = flowRates.filter(_._2 > 0).keySet
    val Some((totalReleasedPressure, _)) = rec(("AA", "AA"), (0, 0), toOpen, Map())

    totalReleasedPressure
  }

  private def generateCandidates(toOpen: Set[String], currentTime: (Int, Int), currentPosition: (String, String), timeLimit: Int, distances: Map[String, Map[String, Int]]): Set[(String, String)] = {
    for {
      first <- toOpen if distances(currentPosition._1)(first) + currentTime._1 < timeLimit
      second <- toOpen if distances(currentPosition._2)(second) + currentTime._2 < timeLimit
      if first != second
    } yield (first, second)
  }
}
