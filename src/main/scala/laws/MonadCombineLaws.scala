package laws

import core.IsEq
import core.IsEq._
import core.MonadCombine
import core.MonadCombine.ops._

trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with ApplicativeLaws[F] with MonoidKLaws[F] {

  implicit val typeClass: MonadCombine[F]

  def leftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEq[F[B]] =
    (fa <+> fa2).flatMap(f) =?= ((fa flatMap f) <+> (fa2 flatMap f))

}

object MonadCombineLaws {

  def apply[F[_]](implicit F0: MonadCombine[F]): MonadCombineLaws[F] = new MonadCombineLaws[F] {
    val typeClass: MonadCombine[F] = MonadCombine[F]
  }

}