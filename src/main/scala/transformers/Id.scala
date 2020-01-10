package transformers

import core.Monad

object Id {

  implicit val monadInstance: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

}
