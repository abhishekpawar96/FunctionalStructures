package core

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def instance[A](empty: A)(combine: (A, => A) => A): Monoid[A] = {
    val empty0 = empty
    val combine0 = combine
    new Monoid[A] {
      def empty: A = empty0
      def combine(x: A, y: => A): A = combine0(x, y)
    }
  }

  implicit val intMonoid: Monoid[Int] = instance[Int](0)(_ + _)
  implicit val stringMonoid: Monoid[String] = instance[String]("")(_ + _)
  implicit val longMonoid: Monoid[Long] = instance[Long](0L)(_ + _)

  implicit def listMonoid[A]: Monoid[List[A]] = instance[List[A]](Nil)(_ ++ _)
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] =
    instance[Option[A]](None)(
      (x, y) =>
        (x, y) match {
          case (None, None) => None
          case (Some(x), None) => Some(x)
          case (None, Some(y)) => Some(y)
          case (Some(x), Some(y)) => Some(Semigroup[A].combine(x, y))
        }
    )

}



