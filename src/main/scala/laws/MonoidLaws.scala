package laws

import core.IsEq
import core.IsEq._
import core.Monoid
import core.Monoid.ops._

trait MonoidLaws[A] extends SemigroupLaws[A] {

  implicit val typeClass: Monoid[A]

  def combineRightIdentity(x: A): IsEq[A] =
    (x |+| typeClass.empty) =?= x

  def combineLeftIdentity(x: A): IsEq[A] =
    (typeClass.empty |+| x) =?= x

}

object MonoidLaws {

  def apply[A: Monoid]: MonoidLaws[A] = new MonoidLaws[A] {
    val typeClass: Monoid[A] = Monoid[A]
  }

}