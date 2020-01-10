package laws

import core.Functor
import core.IsEq
import core.IsEq._

trait FunctorLaws[F[_]] {

  implicit def F: Functor[F]

  def identity[A](fa: F[A]): IsEq[F[A]] =
    F.map(fa)(a => a) =?= fa

  def composition[A, B, C](fa: F[A])(f: A => B)(g: B => C)(implicit F: Functor[F]): IsEq[F[C]] =
    F.map(F.map(fa)(f))(g) =?= F.map(fa)(f.andThen(g))

}

object FunctorLaws {

  def apply[F[_]](implicit F0: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
    def F: Functor[F] = F0
  }

}