package einc.parserc

trait Alternative[F[_]]:
  def fail[X]: F[X]

  extension [A](fa: F[A])
    def <|>[B](fb: => F[B]): F[A | B]

object AlternativeOps:
  extension [F[_], A](fa: F[A])
    def many(using Applicative[F], Alternative[F]): F[List[A]] =
      some <|> Nil.embed

    def some(using Applicative[F], Alternative[F]): F[List[A]] =
      fa.map(x => (xs: List[A]) => x :: xs) <*> many

  def choices[F[_]: Alternative, X](fxs: List[F[X]]): F[X] = fxs match
    case x :: xs => x <|> choices(xs)
    case Nil =>
      val inst = summon[Alternative[F]]
      inst.fail

