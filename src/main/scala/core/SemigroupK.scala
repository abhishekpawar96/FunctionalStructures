package core

import simulacrum.op
import simulacrum.typeclass

import scala.language.higherKinds

@typeclass trait SemigroupK[F[_]] extends Any { self =>

  @op("<+>") def combine[A](x: F[A], y: => F[A]): F[A]

  def toSemigroup[A]: Semigroup[F[A]] =  new Semigroup[F[A]] {
    def combine(x: F[A], y: => F[A]): F[A] = self.combine(x, y)
  }

}

object SemigroupK {

  implicit def listSemigroupK: SemigroupK[List] = new SemigroupK[List] {
    def combine[A](x: List[A], y: => List[A]): List[A] = x ++ y
  }

  implicit def optionSemigroupK: SemigroupK[Option] = new SemigroupK[Option] {
    // TODO: can this be fixed ?
    def combine[A](x: Option[A], y: => Option[A]): Option[A] = x orElse y
  }

}



