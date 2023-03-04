package symsim
package examples.concrete.pumping

import symsim.concrete.Randomized.given
import symsim.CanTestIn.given
import symsim.CanTestIn.*

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.*
import examples.concrete.pumping.*

// To eliminate the warning on Pumping, until scalacheck makes it open
import scala.language.adhocExtensions


class PumpSpec
  extends org.scalacheck.Properties ("Pumping"):

    // Generators of test data
    val flow = Gen.choose[Double] (FlowMin, FlowMax)
    val head = Gen.choose[Double] (HeadMin, HeadMax)
    val head_mean = Gen.choose[Double] (HeadMin, HeadMax)
    val tank = Gen.choose[Double] (TankMin, TankMax)
    val time = Gen.choose[Int] (0, 24)
    val water = Gen.choose[Double] (WaterMin, WaterMax)
    val past_head_mean = Gen.listOfN (5, Gen.choose (HeadMin, HeadMax))
    val actions = Gen.oneOf (Pump.instances.enumAction.membersAscending)
    val obsStates = Gen.oneOf (Pump.instances.allObservableStates)


    // Tests

    property ("The tank level never can be negative") = {
      forAll (flow, head, head_mean, tank, time, water, past_head_mean, actions) {
        (f, h, hm, tl, t, w, phm, a) =>
        for (s1, r) <- Pump.step (PumpState (f, h, hm, tl, t, w, phm)) (a)
        yield s1.tl >= TankMin
      }
    }

    property ("There is an observable state for each valid state in the environment") = {
      forAll (flow, head, head_mean, tank, time, water, past_head_mean, actions) {
        (f, h, hm, tl, t, w, phm, a) =>
          val (s1, r) = Pump.step (PumpState (f, h, hm, tl, t, w, phm)) (a).head
          exists (obsStates) {s => s == Pump.observe (s1)}
      }
    }

    property ("The time can be more than TankMax (comparing is not correct)") = {
      exists (time) { t =>
        t > TankMax
      }
    }

    property ("Test getDemand") = {
      forAll (time) { t1 =>
        exists (time) {t2 => Pump.getDemand (t1) != Pump.getDemand (t2)}
      }
    }

    property ("There is an action for each state which results no overflow") = {
      forAll (flow, head, head_mean, tank, time, water, past_head_mean) {
        (f, h, hm, tl, t, w, phm) =>
          exists(actions) { a =>
            for (s1, r) <- Pump.step(PumpState(f, h, hm, tl, t, w, phm))(a)
              yield (s1.tl <= TankMax)
          }
      }
    }
