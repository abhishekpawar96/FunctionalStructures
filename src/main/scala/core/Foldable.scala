package core

import simulacrum.typeclass

import scala.language.higherKinds

@typeclass trait Foldable[F[_]] extends Any { self =>

  def foldLeft[A, B](fa: F[A], initial: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldLeft(fa, mb.empty)((b, a) => mb.combine(b, f(a)))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(identity)

  def compose[G[_]](implicit G: Foldable[G]): Foldable[Lambda[X => F[G[X]]]] =
    new Foldable[Lambda[X => F[G[X]]]] {
      def foldLeft[A, B](fga: F[G[A]], initial: B)(f: (B, A) => B): B =
        self.foldLeft(fga, initial)((acc, ga) => G.foldLeft(ga, acc)(f))
      def foldRight[A, B](fga: F[G[A]], initial: B)(f: (A, B) => B): B =
        self.foldRight(fga, initial)((ga, acc) => G.foldRight(ga, acc)(f))
    }

  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]) : G[Unit] =
    foldLeft(fa, Applicative[G].pure(())) { (acc, a) =>
      Applicative[G].map2(acc)(f(a))((_, _) => ())
    }

  def sequence_[G[_]: Applicative, A, B](fga: Nested[F, G, A]) : G[Unit] =
    traverse_(fga)(identity)

}

object Foldable {

  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B): B =
      fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B): B =
      fa.foldRight(initial)(f)
  }

  implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](fa: Option[A], initial: B)(f: (B, A) => B): B =
      fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: Option[A], initial: B)(f: (A, B) => B): B =
      fa.foldRight(initial)(f)
  }

}
