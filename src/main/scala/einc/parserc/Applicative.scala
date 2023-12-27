package einc.parserc

trait Applicative[F[_]] extends Functor[F]:
  extension [A, B](ff: F[A => B])
    def <*>(fa: => F[A]): F[B]

object ApplicativeOps:
  extension [F[_]: Applicative, A](fa: F[A])
    def *>[B](fb: F[B]): F[A] =
      fa.map(a => (b: B) => a) <*> fb

    def <*[B](fb: F[B]): F[B] =
      fa.map(a => (b: B) => b) <*> fb

  extension [F[_]: Applicative, A, B](fab: (F[A], F[B]))
    def mapWith[C](f: (A, B) => C): F[C] =
      val (fa, fb) = fab
      fa.map((a: A) => (b: B) => f(a, b)) <*> fb
