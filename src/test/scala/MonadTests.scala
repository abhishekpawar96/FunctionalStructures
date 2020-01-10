import core.Equal
import core.Monad
import laws.MonadLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class MonadTests[F[_]](name: String)(implicit
                                              F: Monad[F],
                                              arbFInt: Arbitrary[F[Int]],
                                              arbFString: Arbitrary[F[String]],
                                              arbFLong: Arbitrary[F[Long]],
                                              arbFLongString: Arbitrary[F[Int => String]],
                                              eqFInt: Equal[F[Int]],
                                              eqFLong: Equal[F[Long]],
                                              eqFString: Equal[F[String]])
  extends Properties(s"core.Monad[$name]"){

  val laws: MonadLaws[F] = MonadLaws[F]

  property("associativity") = forAll { (fa: F[Int], f: Int => F[String], g: String => F[Long]) =>
    laws.associativity(fa)(f)(g).isEqual
  }

  property("leftIdentity") = forAll { (a: Int, f: Int => F[String]) =>
    laws.leftIdentity(a)(f).isEqual
  }

  property("rightIdentity") = forAll { (fa: F[Int]) =>
    laws.rightIdentity(fa).isEqual
  }

}

object ListMonadTest extends MonadTests[List]("List")
object OptionMonadTest extends MonadTests[Option]("Option")