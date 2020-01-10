package core

import simulacrum._

import scala.language.higherKinds

@typeclass trait MonadFilter[F[_]] extends Monad[F] { self =>

  def empty[A]: F[A]

  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    flatMap(fa)(a => if(f(a)) pure[A](a) else empty[A])

  def filterM[A](fa: F[A])(f: A => F[Boolean]): F[A] =
    flatMap(fa)(a => flatMap(f(a))( a1 => if (a1) pure[A](a) else empty[A]))

}

object MonadFilter {

  implicit val listMonadFilter: MonadFilter[List] = new MonadFilter[List] {
    def empty[A]: List[A] = Nil
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonadFilter: MonadFilter[Option] =  new MonadFilter[Option] {
    def empty[A]: Option[A] = None
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

}



