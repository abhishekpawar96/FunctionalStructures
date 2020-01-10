import core.Equal
import core.Semigroup
import laws.SemigroupLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class SemigroupTests[A](name: String)(implicit
                                               F: Semigroup[A],
                                               arb: Arbitrary[A],
                                               eq: Equal[A])
  extends Properties(s"core.Semigroup[$name]"){

  val laws: SemigroupLaws[A] = SemigroupLaws[A]

  property("associativity") = forAll { (x: A, y: A, z: A) =>
    laws.associativity(x, y, z).isEqual
  }

}

object IntSemigroupTest extends SemigroupTests[Int]("Int")
object LongSemigroupTest extends SemigroupTests[Long]("Long")
object StringSemigroupTest extends SemigroupTests[String]("String")
object ListSemigroupTest extends SemigroupTests[List[Int]]("List")
object OptionSemigroupTest extends SemigroupTests[Option[List[Long]]]("Option[List[Long]]")
