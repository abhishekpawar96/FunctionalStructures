package laws

import core.IsEq
import core.IsEq._
import core.MonadFilter
import core.MonadFilter.ops._

trait MonadFilterLaws[F[_]] extends MonadLaws[F] {

  implicit val typeClass: MonadFilter[F]

  import typeClass.empty

  def leftDistributivity[A, B](f: A => F[B]): IsEq[F[B]] =
    empty[A].flatMap(f) =?= empty[B]

  def rightDistributivity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap { a => empty[A] } =?= empty[A]

}

object MonadFilterLaws {

  def apply[F[_]](implicit F0: MonadFilter[F]): MonadFilterLaws[F] = new MonadFilterLaws[F] {
    implicit val typeClass: MonadFilter[F] = F0
  }

}