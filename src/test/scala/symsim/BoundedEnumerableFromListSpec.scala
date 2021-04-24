package symsim

import cats.kernel.BoundedEnumerable
import cats.kernel.laws.discipline.BoundedEnumerableTests
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class BoundedEnumerableFromListSpec extends SymSimSpec:

    given BoundedEnumerable[Unit] = BoundedEnumerableFromList (())

    given BoundedEnumerable[Boolean] = BoundedEnumerableFromList (true, false)

    val l = Seq (1.0, 2.0, 42.0, 0.42)
    given Arbitrary[Double] = Arbitrary[Double] (Gen.oneOf (l))
    given BoundedEnumerable[Double] = BoundedEnumerableFromList (l: _*)

    checkAll ("BoundedEnumerableFromList[Unit]",
      BoundedEnumerableTests[Unit].boundedEnumerable)

    checkAll ("BoundedEnumerableFromList[Boolean]",
      BoundedEnumerableTests[Boolean].boundedEnumerable)

    checkAll ("BoundedEnumerableFromList[4 x Double]",
      BoundedEnumerableTests[Double].boundedEnumerable)
