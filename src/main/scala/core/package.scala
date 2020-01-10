package object core {

  type Nested[F[_], G[_], A] = F[G[A]]

}
