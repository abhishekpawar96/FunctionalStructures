import core.Equal
import core.SemigroupK
import laws.SemigroupKLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class SemigroupKTests[F[_]](name: String)(implicit
                                                   F: SemigroupK[F],
                                                   arb: Arbitrary[F[Int]],
                                                   eq: Equal[F[Int]])
  extends Properties(s"core.SemigroupK[$name]"){

  val laws: SemigroupKLaws[F] = SemigroupKLaws[F]

  property("associativity") = forAll { (x: F[Int], y: F[Int], z: F[Int]) =>
    laws.associativity(x, y, z).isEqual
  }

}

object ListSemigroupKTest extends SemigroupKTests[List]("List")
object OptionSemigroupKTest extends SemigroupKTests[Option]("Option")
