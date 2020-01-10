import core.Applicative
import core.Equal
import laws.ApplicativeLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class ApplicativeTests[F[_]](name: String)(implicit
                                                    F: Applicative[F],
                                                    arbFInt: Arbitrary[F[Int]],
                                                    arbFString: Arbitrary[F[String]],
                                                    arbFLong: Arbitrary[F[Long]],
                                                    arbFLongString: Arbitrary[F[Int => String]],
                                                    eqFInt: Equal[F[Int]],
                                                    eqFLong: Equal[F[Long]],
                                                    eqFString: Equal[F[String]])
  extends Properties(s"core.Applicative[$name]"){

  val laws: ApplicativeLaws[F] = ApplicativeLaws[F]

  property("identity") = forAll { (xs: F[Int]) =>
    laws.identity(xs).isEqual
  }

  property("homomorphism") = forAll { (a: Long, f: Long => Int) =>
    laws.homomorphism(a)(f).isEqual
  }

  property("interchange") = forAll { (a: Int, f: F[Int => String]) =>
    laws.interchange(a)(f).isEqual
  }

  property("map") = forAll { (fa: F[Long], f: Long => String) =>
    laws.map(fa)(f).isEqual
  }

}

object ListApplicativeTest extends ApplicativeTests[List]("List")
object OptionApplicativeTest extends ApplicativeTests[Option]("Option")