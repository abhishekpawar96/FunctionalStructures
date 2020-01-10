package core

import simulacrum.typeclass

import scala.language.higherKinds

@typeclass trait MonoidK[F[_]] extends Any with SemigroupK[F] { self =>

  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] =  new Monoid[F[A]] {
    def empty: F[A] = self.empty
    def combine(x: F[A], y: => F[A]): F[A] = self.combine(x, y)
  }

}

object MonoidK {

  implicit def listMonoidK: MonoidK[List] = new MonoidK[List] {
    def empty[A]: List[A] = Nil
    def combine[A](x: List[A], y: => List[A]): List[A] = x ++ y
  }

  implicit def optionMonoidK: MonoidK[Option] = new MonoidK[Option] {
    def empty[A]: Option[A] = None
    // TODO: can this be fixed ?
    def combine[A](x: Option[A], y: => Option[A]): Option[A] = x orElse y
  }

}



