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
    def <<[B](fb: => F[B]): F[A] = fa >>= (a => fb >>= (_ => a.inject))

  extension [F[_]](inst: Monad[F])
    def asApplicative: Applicative[F] = new Applicative[F]:
      given Monad[F] = inst

      extension [A, B](ff: F[A => B])
        def <*>(fa: => F[A]): F[B] = ff.flatMap: f =>
          fa.map(f)

      def pure[X](x: X): F[X] = x.inject

      extension [A](fa: F[A])
        def map[B](op: A => B): F[B] = fa.flatMap: a =>
          op(a).inject

