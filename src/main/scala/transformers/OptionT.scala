package transformers

import core.Applicative
import core.Functor
import core.Monad

final case class OptionT[F[_], A](value: F[Option[A]]) {

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.map(f(_).value).getOrElse(F.pure(None))))

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    flatMap(s => OptionT(f(s)))

  def isDefined(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isEmpty)

  def applyM[B](f: OptionT[F, A => B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.map2(value)(f.value)((a, o) => o.flatMap(a.map)))

  def applyA[B](f: OptionT[F, A => B])(implicit A: Applicative[F]): OptionT[F, B] =
    OptionT(A.map2(value)(f.value)((a, o) => o.flatMap(a.map)))

}

object OptionT {

  implicit def functorInstance[F[_]](implicit F: Functor[F]): Functor[OptionT[F, *]] =
    new Functor[OptionT[F, *]] {
      def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)
    }

  implicit def monadInstance[F[_]](implicit F: Monad[F]): Monad[OptionT[F, *]] =
    new Monad[OptionT[F, *]] {
      def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa.flatMap(f)
    }

}
