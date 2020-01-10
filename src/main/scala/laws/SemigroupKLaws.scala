package laws

import core.IsEq
import core.IsEq._
import core.SemigroupK
import core.SemigroupK.ops._

trait SemigroupKLaws[F[_]] {

  implicit val typeClass: SemigroupK[F]

  def associativity[A](x: F[A], y: F[A], z: F[A]): IsEq[F[A]] =
    ((x <+> y) <+> z) =?= (x <+> (y <+> z))
}

object SemigroupKLaws {

  def apply[F[_]: SemigroupK]: SemigroupKLaws[F] = new SemigroupKLaws[F] {
    val typeClass: SemigroupK[F] = SemigroupK[F]
  }

}