package laws

import core.IsEq
import core.IsEq._
import core.Monad
import core.Monad.ops._

trait MonadLaws[F[_]] {

  implicit val typeClass: Monad[F]

  import typeClass.pure

  def associativity[A, B, C](fa: F[A])(f: A => F[B])(g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap(a => f(a).flatMap(b => g(b)))

  def leftIdentity[A, B](a: A)(f: A => F[B]): IsEq[F[B]] =
    pure(a).flatMap(f) =?= f(a)

  def rightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(a => pure(a)) =?= fa

}

object MonadLaws {

  def apply[F[_]](implicit F0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    val typeClass: Monad[F] = F0
  }

}