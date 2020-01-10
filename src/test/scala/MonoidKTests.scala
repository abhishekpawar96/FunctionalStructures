import core.Equal
import core.MonoidK
import laws.MonoidKLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class MonoidKTests[F[_]](name: String)(implicit
                                                F: MonoidK[F],
                                                arb: Arbitrary[F[Int]],
                                                eq: Equal[F[Int]])
  extends Properties(s"core.MonoidK[$name]"){

  val laws: MonoidKLaws[F] = MonoidKLaws[F]

  property("associativity") = forAll { (x: F[Int], y: F[Int], z: F[Int]) =>
    laws.associativity(x, y, z).isEqual
  }

  property("leftIdentity") = forAll { (x: F[Int]) =>
    laws.leftIdentityK(x).isEqual
  }

  property("rightIdentity") = forAll { (x: F[Int]) =>
    laws.rightIdentityK(x).isEqual
  }
}

object ListMonoidKTest extends MonoidKTests[List]("List")
object OptionMonoidKTest extends MonoidKTests[Option]("Option")
