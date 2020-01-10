package transformers

import core.Applicative
import core.Functor
import core.Monad
import core.Semigroup

final case class EitherT[F[_], L, R](value: F[Either[L, R]]) {

  def map[R1](f: R => R1)(implicit F: Functor[F]): EitherT[F, L, R1] =
    EitherT(F.map(value)(_.map(f)))

  def flatMap[R1](f: R => EitherT[F, L, R1])(implicit F: Monad[F]): EitherT[F, L, R1] =
    EitherT {
      F.flatMap(value) {
        case Right(v) => f(v).value
        case Left(e) => F.pure(Left(e))
      }
    }

  def flatMapF[R1](f: R => F[Either[L, R1]])(implicit F: Monad[F]): EitherT[F, L, R1] =
    flatMap(s => EitherT(f(s)))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def applyM[R1](f: EitherT[F, L, R => R1])(implicit F: Monad[F]): EitherT[F, L, R1] =
    EitherT(F.map2(value)(f.value)((a, o) => o.flatMap(a.map)))

  def applyM[R1](f: EitherT[F, L, R => R1])(implicit F: Applicative[F]): EitherT[F, L, R1] =
    EitherT(F.map2(value)(f.value)((a, o) => o.flatMap(a.map)))

  def +++(e: EitherT[F, L, R])(implicit l: Semigroup[L], r: Semigroup[R], F: Applicative[F]): EitherT[F, L, R] =
    EitherT {
      F.map2(value)(e.value) {
        (e1, e2) => {
          (e1, e2) match {
            case (Left(v1), Left(v2)) => Left(l.combine(v1, v2))
            case (Left(v1), Right(v2)) => Right(v2)
            case (Right(v1), Left(v2)) => Right(v1)
            case (Right(v1), Right(v2)) => Right(r.combine(v1, v2))
          }
        }
      }
    }

}

object EitherT {

  def monadInstance[F[_], L](implicit F: Monad[F]): Monad[EitherT[F, L, *]] =
    new Monad[EitherT[F, L, *]] {
      def pure[R](a: R): EitherT[F, L, R] = EitherT(F.pure(Right(a)))
      def flatMap[R, R1](fa: EitherT[F, L, R])(f: R => EitherT[F, L, R1]): EitherT[F, L, R1] =
        fa.flatMap(f)
    }

  def functorInstance[F[_], L](implicit F: Functor[F]): Functor[EitherT[F, L, *]] =
    new Functor[EitherT[F, L, *]] {
      def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa.map(f)
    }

}