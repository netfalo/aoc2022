import scala.io.Source

case class Resource(name: String) {
  def content: String = Source.fromResource(name)
    .getLines()
    .mkString("\n")

  def getLines: Array[String] = Source.fromResource(name)
    .getLines
    .toArray
}
