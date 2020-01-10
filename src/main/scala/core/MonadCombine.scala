package core

import simulacrum._

import scala.language.higherKinds

@typeclass trait MonadCombine[F[_]] extends MonadFilter[F] with Applicative[F] with MonoidK[F] { self =>

  def unite[G[_]: Foldable, A](fga: Nested[F, G, A]): F[A] =
    flatMap(fga)(ga => Foldable[G].foldMap(ga)(a => pure(a))(toMonoid[A]))

}

object MonadCombine {

  implicit val listMonadCombine: MonadCombine[List] = new MonadCombine[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def empty[A]: List[A] = Nil
    def combine[A](x: List[A], y: => List[A]): List[A] = x ++ y
  }

  implicit val optionMonadCombine: MonadCombine[Option] = new MonadCombine[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    def empty[A]: Option[A] = None
    def combine[A](x: Option[A], y: => Option[A]): Option[A] = x orElse y
  }

}



