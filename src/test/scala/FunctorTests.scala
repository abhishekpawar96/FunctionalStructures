import core.Equal
import core.Functor
import laws.FunctorLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class FunctorTests[F[_]](name: String)(implicit
                                                F: Functor[F],
                                                arbFInt: Arbitrary[F[Int]],
                                                eqFInt: Equal[F[Int]],
                                                eqFLong: Equal[F[Long]])
  extends Properties(s"core.Functor[$name]"){

  val laws: FunctorLaws[F] = FunctorLaws[F]

  property("identity") = forAll { (xs: F[Int]) =>
    laws.identity(xs).isEqual
  }

  property("composition") = forAll {
    (xs: F[Int], f: Int => String, g: String => Long) =>
      laws.composition(xs)(f)(g).isEqual
  }

}

object ListFunctorTest extends FunctorTests[List]("List")
object OptionFunctorTest extends FunctorTests[Option]("Option")