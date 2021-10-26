package symsim
package examples.concrete.breaking

import CanTestIn._

import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest._
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean, exists}
import examples.concrete.breaking.CarState
import examples.concrete.breaking.Car

/** Sanity tests for Randomized as a Scheduler */
class CarSpec
   extends org.scalatest.freespec.AnyFreeSpec
   with org.scalatestplus.scalacheck.Checkers:

   "Sanity checks for symsim.concrete.breaking" - {

       val positions = Gen.choose[Double] (0.0, 10.0)

       "The car cannot move backwards" in check {
          forAll (positions) { p =>
             forAll { (a: Double) =>
               val (s1, r) = Car.step (CarState (v = 0.0, p = p)) (a).head
               (a <= 0) ==> (s1.p >= p)
             }
          }
       }

       "The car cannot move backwards (regression)" in {
          val p = 1.0
          val a = 0.0
          val (s1, r) = Car.step (CarState (v = 0.0, p = p)) (a).head
          assert (s1.p >= p)
       }

       val gen01 = Gen.choose[Double] (0.0, 1.0)
       val decels = Gen.choose[Double] (-10.0, -5.0)

       "Position never becomes negative" in check {
          forAll (gen01, positions, decels) { (v, p, a) =>
             val (s1, r) = Car.step (CarState (v, p)) (a).head
             s1.p >= 0.0
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
// TODO:        arg0 = (1.5114558155620142E-5,1.821453015080769E-5), // 36 shrinks
// [info]       arg1 = 0.0, // 19 shrinks
// [info]       arg2 = -1.8141283195730265E-5 // 19 shrinks
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
