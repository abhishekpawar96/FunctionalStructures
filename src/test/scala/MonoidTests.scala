import core.Equal
import core.Monoid
import laws.MonoidLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.language.higherKinds

abstract class MonoidTests[A](name: String)(implicit
                                                    F: Monoid[A],
                                                    arb: Arbitrary[A],
                                                    eq: Equal[A])
  extends Properties(s"core.Monoid[$name]"){

  val laws: MonoidLaws[A] = MonoidLaws[A]

  property("combineLeftIdentity") = forAll { (x: A) =>
    laws.combineLeftIdentity(x).isEqual
  }

  property("combineRightIdentity") = forAll { (x: A) =>
    laws.combineRightIdentity(x).isEqual
  }

}

object IntMonoidTest extends MonoidTests[Int]("Int")
object LongMonoidTest extends MonoidTests[Long]("Long")
object StringMonoidTest extends MonoidTests[String]("String")
object ListMonoidTest extends MonoidTests[List[Int]]("List")
object OptionMonoidTest extends MonoidTests[Option[List[Long]]]("Option[List[Long]]")
