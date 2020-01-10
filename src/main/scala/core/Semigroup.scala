package core

import simulacrum.op
import simulacrum.typeclass

@typeclass trait Semigroup[A] extends Any {
  @op("|+|") def combine(x: A, y: => A): A
}

object Semigroup {

  def instance[A](combine: (A, => A) => A): Semigroup[A] = {
    val combine0 = combine
    new Semigroup[A] {
      def combine(x: A, y: => A): A = combine0(x, y)
    }
  }

  implicit val intSemigroup: Semigroup[Int] = instance[Int](_ + _)
  implicit val stringSemigroup: Semigroup[String] = instance[String](_ + _)
  implicit val longSemigroup: Semigroup[Long] = instance[Long](_ + _)

  implicit def listSemigroup[A]: Semigroup[List[A]] = instance[List[A]](_ ++ _)
  implicit def optionSemigroup[A: Semigroup]: Semigroup[Option[A]] =
    instance[Option[A]](
      (x, y) =>
        (x, y) match {
          case (None, None) => None
          case (Some(x), None) => Some(x)
          case (None, Some(y)) => Some(y)
          case (Some(x), Some(y)) => Some(Semigroup[A].combine(x, y))
        }
    )

}



