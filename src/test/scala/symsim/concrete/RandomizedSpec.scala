package symsim
package concrete

import CanTestIn._

import org.scalacheck.Prop.{forAll, propBoolean}
import symsim.concrete.Randomized.canTestInRandomized

/** Sanity tests for Randomized as a Scheduler */
class RandomizedSpec extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatestplus.scalacheck.Checkers {

  "Sanity checks for symsim.concrete.Randomized" - {

    "between Double observes the bounds" in check {
      forAll { (mn: (Double, Double)) =>
        val m = Math.min (mn._1, mn._2)
        val n = Math.max (mn._1, mn._2)
        val between = Randomized.between (m, n)
        m != n ==>
          forAll (between.toGen) { x => m <= x && x < n }
      }
    }

    "between Int observes the bounds" in check {
      forAll { (mn: (Int, Int)) =>
        val m = Math.min (mn._1, mn._2)
        val n = Math.max (mn._1, mn._2)
        val between = Randomized.between (m, n)
        m != n ==>
          forAll (between.toGen) { x => m <= x && x < n }
      }
    }

  }

}
