import java.time.Instant
import java.time.Duration
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside

class AnyFlatSpec extends org.scalatest.flatspec.AnyFlatSpec
    with Matchers
    with org.scalatest.BeforeAndAfter
    with Inside {

  val start = Instant.now()

  after {
    println(Duration.between(Instant.now(), start))
  }
}
