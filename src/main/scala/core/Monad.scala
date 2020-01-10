package core

import simulacrum._

import scala.language.higherKinds

@typeclass trait Monad[F[_]] extends Applicative[F] { self =>

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(f)((f:A => B) => map(fa)(f))

  def flatten[A](fa: Nested[F, F, A]): F[A] =
    flatMap(fa)(identity)

}

object Monad {

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def pure[A](a: A): List[A] = List(a)
  }

  implicit val optiontMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def pure[A](a: A): Option[A] = Option(a)
  }

}



