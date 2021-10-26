package symsim
package examples.concrete.breaking

import CanTestIn._

import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest._
import org.scalacheck.Prop.{forAll, propBoolean,exists}
import examples.concrete.breaking.CarState
import examples.concrete.breaking.Car

/** Sanity tests for Randomized as a Scheduler */
class RandomizedSpec extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatestplus.scalacheck.Checkers:

  "Sanity checks for symsim.concrete.breaking" - {

    val positions=Gen.choose[Double] (0.0,10.0)

    "Checking position for no backward movement" in check {
      forAll  (positions) {p=>
        forAll{(a: Double) =>
          val s1 = Car.step(CarState(0.0,p))(a).head._1
          true ==> s1.p >= p
      }
     }
    }

    val GenPair : Gen[(Double, Double)] = for {
      v <- Gen.choose[Double] (0.0,1.0)
      p <- Gen.choose[Double] (0.0,1.0)
    } yield (v, p)

    val decels = Gen.choose[Double] (-10.0,-5.0)
    "Position never becomes negative" in check {
      forAll (GenPair) { vp=>
        forAll (decels) {a=>
          val s1 = Car.step(CarState(vp._1,vp._2))(a).head._1
          true ==> s1.p >= 0.0
      }
     }
    }

    val Velocity = Gen.choose[Double] (0.0,10.0)

    val Deceleration = Gen.choose[Double] (-10.0,0.0)

      val Pairs : Gen[(Double, Double)] = for {
      p1 <- Gen.choose[Double] (0.0,10.0)
      p2 <- Gen.choose[Double] (0.0,10.0)
    } yield (p1, p2)

    "Reward is valid 1" in check {
      forAll  (Pairs) { posPair=>
        forAll (Velocity){v=>
          forAll (Deceleration) { a=>
            val r1 = Car.step(CarState(v,posPair._1))(a).head._2
            val r2 = Car.step(CarState(v,posPair._2))(a).head._2
            posPair._1 <= posPair._2 ==> r1 >= r2
       }
      }
     }
    }

    "Reward is valid 2" in check {
      forAll  (Pairs) { velPair=>
        forAll (positions){p=>
          forAll (Deceleration) { a=>
            val r1 = Car.step(CarState(velPair._1,p))(a).head._2
            val r2 = Car.step(CarState(velPair._2,p))(a).head._2
            velPair._1 <= velPair._2 ==> r1 >= r2
       }
      }
     }
    }
  }
