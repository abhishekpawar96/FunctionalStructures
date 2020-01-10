package core

import simulacrum.typeclass

import scala.language.higherKinds

@typeclass trait Traverse[F[_]] extends Any with Functor[F] with Foldable[F] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: Nested[F, G, A]): G[F[A]] =
    traverse(fga)(identity)

  def compose[G[_]: Traverse](implicit G: Traverse[G]): Traverse[Lambda[X => F[G[X]]]] =
    new Traverse[Lambda[X => F[G[X]]]] {
      def traverse[H[_] : Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        self.traverse(fga)(ga => G.traverse(ga)(f))
      def foldLeft[A, B](fga: F[G[A]], initial: B)(f: (B, A) => B): B =
        self.foldLeft(fga, initial)((acc, ga) => G.foldLeft(ga, acc)(f))
      def foldRight[A, B](fga: F[G[A]], initial: B)(f: (A, B) => B): B =
        self.foldRight(fga, initial)((ga, acc) => G.foldRight(ga, acc)(f))
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fga)(G.map(_)(f))
    }

}

object Traverse {

  implicit val listTraverse: Traverse[List] = new Traverse[List] {
    def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.reverse.foldLeft(Applicative[G].pure(Nil: List[B])) {
        (acc, a) =>
          Applicative[G].map2(f(a))(acc)(_ :: _)
    }
    def foldLeft[A, B](fa: List[A], initial: B)(f: (B, A) => B): B = fa.foldLeft(initial)(f)
    def foldRight[A, B](fa: List[A], initial: B)(f: (A, B) => B): B = fa.foldRight(initial)(f)
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

}
