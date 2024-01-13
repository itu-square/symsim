package symsim
package concrete

import scala.util.Try
import cats.syntax.all.*

import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}
import org.scalacheck.Gen

import symsim.concrete.Randomized2.canTestInRandomized
import symsim.CanTestIn.*

/** Sanity tests for Randomized2 as a Scheduler */
class RandomizedSpec extends 
  org.scalatest.freespec.AnyFreeSpec,
  org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  val C = 5000

  // "Sanity checks for symsim.concrete.Randomized2" - {

  //  "between Double observes the bounds" in check {
  //    forAll { (mn: (Double, Double)) =>
  //      val m = Math.min (mn._1, mn._2)
  //      val n = Math.max (mn._1, mn._2)
  //      val between = Randomized2.between (m, n)
  //      m != n ==>
  //        forAll (between.toGen) { x => m <= x && x < n }
  //    }
  //  }

  //  "between Int observes the bounds" in check {
  //    forAll { (mn: (Int, Int)) =>
  //      val m = Math.min (mn._1, mn._2)
  //      val n = Math.max (mn._1, mn._2)
  //      m != n ==>
  //        forAll (Randomized2.between (m, n).toGen) { x => m <= x && x < n }
  //    }
  //  }
  // }

  "Regressions" - {

    def repeatNotConstant[T: Numeric] (ra: =>Randomized2[T]): Boolean =
       Try { ra.sample (20)
               .toList
               .distinct
               .size > 1 }.get

    def repeatNotConstantB (ra: => Randomized2[Boolean]): Boolean =
      repeatNotConstant[Int] (ra map { x => if x then 1 else 0 })

    "20 consecutive values of const(42)* are not different" in {
       assert { !repeatNotConstant (Randomized2.const (42)) }
    }

    "20 consecutive values of between(1,100)* are random (different)" in {
       assert { repeatNotConstant (Randomized2.between (1,100)) }
    }

    "20 consecutive values of between(-1.0,1.0)* are random (different)" in {
       assert { repeatNotConstant (Randomized2.between (1.0, 100.0)) }
    }

    "20 consecutive values of coin(.5)* are random (different)" in {
       assert { repeatNotConstantB (Randomized2.coin (.5)) }
    }

    "20 consecutive values of coin(.1)* are not different" in {
       assert { !repeatNotConstantB (Randomized2.coin (1.0)) }
    }

    "20 consecutive values of oneOf(1..100)* are random (different)" in {
       assert { repeatNotConstant (Randomized2.oneOf (1 to 100*)) }
    }

    /* This test records what is the problem with referential transparency in
     * the current implementation of Randomized2.
     */
    "Randomized2 is referentially transparent" in check {
        val rng = spire.random.rng.Serial (42)
        Randomized2.between(1,100).sample (10) (using rng).toList ==
        Randomized2.between(1,100).sample (10) (using rng).toList
    }

    "Randomized2.gaussian (m, d) has stddev 'd' and mean 'm'" in check {

      val n = 20000
      val epsilon = 0.06

      forAllNoShrink (Gen.choose (-100.0, +100.0), Gen.choose(0.1, 2.0)) {
        (m: Double, d: Double) =>
          Math.abs (m) >= 0.5 ==> {
            val sample = Randomized2.gaussian (mean = m, stdDev = d).sample (n)
            val mean = sample.sum / n
            val variance = sample.map { x => (x-mean)*(x-mean) }.sum / n
            val mm = Math.abs ((mean-m)/ m)
            val vd = Math.abs ((Math.sqrt (variance) - d) / d)
            (s"High relative error of Mean ${mm} (>)"   |: mm <= epsilon) &&
            (s"High relative error of StdDev ${vd} (>)" |: vd <= epsilon) 
          }
      }
    }
  }

end RandomizedSpec
