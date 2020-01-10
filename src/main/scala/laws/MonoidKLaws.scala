package laws

import core.IsEq
import core.IsEq._
import core.MonoidK
import core.MonoidK.ops._

trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {

  implicit val typeClass: MonoidK[F]

  def rightIdentityK[A](x: F[A]): IsEq[F[A]] =
    (x <+> typeClass.empty) =?= x

  def leftIdentityK[A](x: F[A]): IsEq[F[A]] =
    (typeClass.empty <+> x) =?= x

}

object MonoidKLaws {

  def apply[F[_]: MonoidK]: MonoidKLaws[F] = new MonoidKLaws[F] {
    val typeClass: MonoidK[F] = MonoidK[F]
  }

}