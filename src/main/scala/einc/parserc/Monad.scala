package einc.parserc

trait Monad[F[_]]:
  def unit[X](x: X): F[X]

  extension [A](fa: F[A])
    def flatMap[B](op: A => F[B]): F[B]
    def >>=[B](op: A => F[B]): F[B] = flatMap(op)

  extension [X](x: X)
    def inject: F[X] = unit(x)

object MonadOps:
  extension [F[_]: Monad, A](fa: F[A])
    def >>[B](fb: => F[B]): F[B] = fa >>= (_ => fb)

