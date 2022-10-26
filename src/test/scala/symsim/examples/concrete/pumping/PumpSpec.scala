package symsim
package examples.concrete.pumping

import CanTestIn.*

import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest.*
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean, exists}
import examples.concrete.pumping.*

/** Sanity tests for Randomized as a Scheduler */
class PumpSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:


  "Sanity checks for symsim.concrete.pumping" - {

    // Generators of test data
    val flow = Gen.choose[Double] (FLOW_MIN, FLOW_MAX)
    val head = Gen.choose[Double] (HEAD_MIN, HEAD_MAX)
    val head_mean = Gen.choose[Double] (HEAD_MIN, HEAD_MAX)
    val tank = Gen.choose[Double] (TANK_MIN, TANK_MAX)
    val time = Gen.choose[Int] (0, 24)
    val water = Gen.choose[Double] (WATER_MIN, WATER_MAX)
    val past_head_mean = Gen.listOfN (5, Gen.choose (HEAD_MIN, HEAD_MAX))
    val actions = Gen.oneOf (Pump.instances.enumAction.membersAscending)
    val obsStates = Gen.oneOf (Pump.instances.allObservableStates)


    // Tests

    "The tank level never can be negative" in check {
      forAll (flow, head, head_mean, tank, time, water, past_head_mean, actions) {
        (f, h, hm, tl, t, w, phm, a) =>
        val (s1, r) = Pump.step (PumpState (f, h, hm, tl, t, w, phm)) (a).head
        s1.tl >= TANK_MIN
      }
    }

    "There is an observable state for each valid state in the environment" in check {
      forAllNoShrink (flow, head, head_mean, tank, time, water, past_head_mean, actions) {
        (f, h, hm, tl, t, w, phm, a) =>
          val (s1, r) = Pump.step (PumpState (f, h, hm, tl, t, w, phm)) (a).head
          exists (obsStates) {s => s == Pump.discretize (s1)}
      }
    }

    "The time can be more than TANK_MAX (comparing is not correct)" in check {
      exists (time) { t =>
        t > TANK_MAX
      }
    }

    "Test getDemand" in check {
      forAllNoShrink (time) { t1 =>
        exists (time) {t2 => Pump.getDemand (t1) != Pump.getDemand (t2)}
      }
    }

  }
