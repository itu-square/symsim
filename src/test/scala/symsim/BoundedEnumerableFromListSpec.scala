package symsim

import cats.kernel.BoundedEnumerable
import cats.kernel.laws.discipline.BoundedEnumerableTests
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class BoundedEnumerableFromListSpec extends SymSimSpec {

    implicit val evUnit: BoundedEnumerable[Unit] =
      BoundedEnumerableFromList (())

    implicit val evBoolean: BoundedEnumerable[Boolean] =
      BoundedEnumerableFromList (true, false)

    val l = Seq (1.0, 2.0, 42.0, 0.42)
    implicit val arbDouble = Arbitrary[Double] { Gen.oneOf (l) }
    implicit val evDouble: BoundedEnumerable[Double] =
      BoundedEnumerableFromList (l: _*)

    checkAll ("BoundedEnumerableFromList[Unit]",
      BoundedEnumerableTests[Unit].boundedEnumerable)

    checkAll ("BoundedEnumerableFromList[Boolean]",
      BoundedEnumerableTests[Boolean].boundedEnumerable)

    checkAll ("BoundedEnumerableFromList[4 x Double]",
      BoundedEnumerableTests[Double].boundedEnumerable)

}
