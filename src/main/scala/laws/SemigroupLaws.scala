package laws

import core.IsEq
import core.IsEq._
import core.Semigroup
import core.Semigroup.ops._

trait SemigroupLaws[A] {

  implicit val typeClass: Semigroup[A]

  def associativity(x: A, y: A, z: A): IsEq[A] =
    ((x |+| y) |+| z) =?= (x |+| (y |+| z))

}

object SemigroupLaws {

  def apply[A: Semigroup]: SemigroupLaws[A] = new SemigroupLaws[A] {
    val typeClass: Semigroup[A] = Semigroup[A]
  }

}