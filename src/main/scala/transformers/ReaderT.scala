package transformers

import core.Applicative
import core.Functor
import core.Monad

final case class ReaderT[F[_], A ,B](run: A => F[B]) {

  def map[C](f: B => C)(implicit F: Functor[F]): ReaderT[F, A, C] =
    ReaderT(a => F.map(run(a))(f))

  def flatMap[C](f: B => ReaderT[F, A, C])(implicit F: Monad[F]): ReaderT[F, A, C] =
    ReaderT(a => F.flatMap(run(a))(b => f(b).run(a)))

  def andThen[C](f: ReaderT[F, B, C])(implicit F: Monad[F]): ReaderT[F, A, C] =
    ReaderT(a => F.flatMap(run(a))(f.run))

  def compose[C](f: ReaderT[F, C, A])(implicit F: Monad[F]): ReaderT[F, C, B] =
    f.andThen(this)

}

object ReaderT {

  def ask[F[_], A](implicit F: Applicative[F]): ReaderT[F, A, A] =
    ReaderT(a => F.pure(a))

  implicit def monadInstance[F[_], R](implicit F: Monad[F]): Monad[ReaderT[F, R, *]] = new Monad[ReaderT[F, R, *]] {
    def pure[A](a: A): ReaderT[F, R, A] = ReaderT(_ => F.pure(a))
    def flatMap[A, B](fa: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] =
      fa.flatMap(f)
  }

}

