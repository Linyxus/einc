package einc.parserc

class FunctorSuite extends munit.FunSuite:
  test("functor: list"):
    given Functor[List] with
      def pure[X](x: X): List[X] = x :: Nil
      extension[X] (xs: List[X])
        def map[Y](f: X => Y): List[Y] = xs.map(f)

    assertEquals(1.embed, 1 :: Nil)

    val xs = List(1, 2, 3)
    val inc = (x: Int) => x + 1
    assertEquals(inc <#> xs, List(2, 3, 4))

