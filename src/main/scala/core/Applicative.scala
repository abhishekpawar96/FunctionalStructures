package core

import simulacrum._

import scala.language.higherKinds

@typeclass trait Applicative[F[_]] extends Functor[F] { self =>

  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A])(fb: F[B])(f: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(f)(f => b => a => f(a, b))))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A])(fb: F[B])(f: (A ,B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A])(fb: F[B])(fc: F[C])(f: (A ,B, C) => Z): F[Z] =
    apply(fa)(map2(fb)(fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, Z](fa: F[A])(fb: F[B])(fc: F[C])(fd: F[D])(f: (A ,B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb))(tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d)}

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa)(fb)((a, b) => (a, b))

  def tuple3[A, B, C](fa: F[A])(fb: F[B])(fc: F[C]): F[(A, B, C)] =
    map3(fa)(fb)(fc)((a, b, c) => (a, b, c))

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))
      def apply[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
        self.apply(fga)(self.map(f)(ff => fa => G.apply(fa)(ff)))
    }

}

object Applicative {

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)
    def apply[A, B](fa: List[A])(fs: List[A => B]): List[B] = for {
      a <- fa
      f <- fs
    } yield f(a)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def apply[A, B](fa: Option[A])(f: Option[A => B]): Option[B] =
      (fa, f) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a),Some(f)) => Some(f(a))
      }
  }

}
