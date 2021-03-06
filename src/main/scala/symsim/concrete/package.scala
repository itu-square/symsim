package symsim

import org.scalacheck.Gen
import org.scalacheck.Arbitrary._


package object concrete {

  type Probability = Double
  type Seed = Long

  /** A purely functional wrapping of scala.util.Random */
  type Randomized[A] = cats.data.State[scala.util.Random,A]


  implicit class RandomizedOps[A] (ra: Randomized[A]) {

    def toGen: Gen[A] = for {
      n <- arbitrary[Long]
      r = new scala.util.Random (n)
      a = ra.runA (r).value
    } yield a

  }

}
