package einc.parserc

trait Functor[F[_]]:
  def pure[X](x: X): F[X]

  extension [A] (fa: F[A])
    def map[B](op: A => B): F[B]

  extension [X] (x: X)
    def embed: F[X] = pure(x)

  extension [A, B] (f: A => B)
    def <#> (fa: F[A]): F[B] = fa.map(f)

object FunctorOps:
  extension [F[_]: Functor, A] (fa: F[A])
    def #>[B](b: B): F[B] = fa.map(_ => b)
