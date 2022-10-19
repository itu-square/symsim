package symsim
package concrete

import scala.util.Try

import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}
import org.scalacheck.Gen

import symsim.concrete.Randomized.canTestInRandomized
import symsim.CanTestIn.*

/** Sanity tests for Randomized as a Scheduler */
class RandomizedSpec 
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  val C = 5000

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
       m != n ==>
         forAll (Randomized.between (m, n).toGen) { x => m <= x && x < n }
     }
   }
  }

  "Regressions" - {

    def isSingleton[A] (ra: Randomized[A]): Boolean =
       Try { !ra.isEmpty && ra.tail.isEmpty }.get

    "Randomized.const is finite (a regression)" in {
       assert { isSingleton (Randomized.const(1)) }
    }

    "Randomized.between(Int, Int) is finite (a regression)" in {
       assert { isSingleton (Randomized.between(1, 42)) }
    }

    "Randomized.between(Double, Double) is finite (a regression)" in {
       assert { isSingleton (Randomized.between(-1.0, 2.0)) }
    }

    "Randomized.coin(Double) is finite (a regression)" in {
       assert { isSingleton (Randomized.coin(0.5)) }
    }

    "Randomized.oneOf(Int*) is finite (a regression)" in {
       assert { isSingleton (Randomized.oneOf(1,2,3,5)) }
    }

    def repeatNotConstant[T: Numeric] (ra: =>Randomized[T]): Boolean =
       Try { Randomized.repeat (ra)
          .take (20)
          .toList
          .distinct
          .size > 1 }.get

    def repeatNotConstantB (ra: => Randomized[Boolean]): Boolean =
      repeatNotConstant[Int] (ra map { x => if x then 1 else 0 })

    "20 consecutive values of const(42)* are not different" in {
       assert { !repeatNotConstant (Randomized.const (42)) }
    }

    "20 consecutive values of between(1,100)* are random (different)" in {
       assert { repeatNotConstant (Randomized.between (1,100)) }
    }

    "20 consecutive values of between(-1.0,1.0)* are random (different)" in {
       assert { repeatNotConstant (Randomized.between (1.0, 100.0)) }
    }

    "20 consecutive values of coin(.5)* are random (different)" in {
       assert { repeatNotConstantB (Randomized.coin (.5)) }
    }

    "20 consecutive values of coin(.1)* are not different" in {
       assert { !repeatNotConstantB (Randomized.coin (1.0)) }
    }

    "20 consecutive values of oneOf(1..100)* are random (different)" in {
       assert { repeatNotConstant (Randomized.oneOf (1 to 100*)) }
    }

    /* This test records what is the problem with referential transparency in
     * the current implementation of Randomized.
     */
    "Randomized is referentially transparent (failing)" ignore {
        assert (
           Randomized.repeat (Randomized.between(1,100)).take (10).toList ==
           Randomized.repeat (Randomized.between(1,100)).take (10).toList
        )
    }

    "Randomized.gaussian (m, d) has stddev 'd' and mean 'm'" in check {

       val n = 10000
       val epsilon = 0.05

       forAllNoShrink (Gen.choose (-100.0, +100.0), Gen.choose(0.001,+3.0)) {
          (m: Double, d: Double) =>
             val sample = Randomized.repeat (Randomized.gaussian (mean = m, stddev = d)).take (n)
             val mean = sample.sum / n
             val variance = sample.map { x => (x-mean)*(x-mean) }.sum / n
             ("Mean"   |: Math.abs (mean/m - 1.0) <= epsilon) &&
             ("StdDev" |: Math.abs (Math.sqrt (variance)/d - 1.0) <= epsilon)
       }
    }

  }

  "Randomized Foldable Instance" - {

    "check that foldLeft works (sum)" in {
       val r = Randomized.repeat (Randomized.oneOf (1)).take (C)
       val sum = Randomized.randomizedIsFoldable
          .foldLeft[Int, Int] (r, 0) { _ + _ }
       assert (sum == C)
    }
  }

end RandomizedSpec
