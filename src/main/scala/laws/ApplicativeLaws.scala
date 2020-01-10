package laws

import core.Applicative
import core.Applicative.ops._
import core.IsEq
import core.IsEq._

trait ApplicativeLaws[F[_]] {

  implicit def typeClass: Applicative[F]

  def identity[A](fa: F[A]): IsEq[F[A]] =
    fa.apply(typeClass.pure((a: A) => a)) =?= fa

  def homomorphism[A, B](a: A)(f: A => B): IsEq[F[B]] =
    typeClass.pure(a).apply(typeClass.pure(f)) =?= typeClass.pure(f(a))

  def interchange[A, B](a: A)(ff: F[A => B]): IsEq[F[B]] =
    typeClass.pure(a).apply(ff) =?= ff.apply(typeClass.pure(f => f(a)))

  def map[A, B](fa: F[A])(f: A => B): IsEq[F[B]] =
    fa.map(f) =?= fa.apply(typeClass.pure(f))

}

object ApplicativeLaws {

  def apply[F[_]](implicit F0: Applicative[F]): ApplicativeLaws[F] = new ApplicativeLaws[F] {
    def typeClass: Applicative[F] = F0
  }

}