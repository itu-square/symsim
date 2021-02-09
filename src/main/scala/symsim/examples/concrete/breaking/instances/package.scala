package symsim.examples.concrete.breaking

import cats.Eq

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

package object instances {

  val genCarState: Gen[CarState] =
    for {
      v <- arbitrary[Double]
      p <- arbitrary[Double]
    } yield CarState (Math.abs (v), Math.abs (p))

  implicit val arbitraryCarState: Arbitrary[CarState] = Arbitrary (genCarState)

  implicit val eqCarState: Eq[CarState] = Eq.fromUniversalEquals
}
