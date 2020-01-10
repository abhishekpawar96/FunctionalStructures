package transformers

import core.Monad

final case class Reader[A ,B](run: A => B) {

  def map[C](f: B => C): Reader[A, C] =
    Reader(run.andThen(f))

  def flatMap[C](f: B => Reader[A, C]): Reader[A, C] =
    Reader(a => f(run(a)).run(a))

  def andThen[C](f: Reader[B, C]): Reader[A, C] =
    Reader(run.andThen(f.run))

  def compose[C](f: Reader[C, A]): Reader[C, B] =
    Reader(f.run.andThen(run))

}

object Reader {

  def ask[A]: Reader[A, A] = Reader(identity)

  implicit def monadInstance[R]: Monad[Reader[R, *]] = new Monad[Reader[R, *]] {
    def pure[A](a: A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      fa.flatMap(f)
  }

}