package symsim
package examples.concrete.braking

import symsim.concrete.Randomized.given
import CanTestIn.given

import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest.*
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean, exists}
import examples.concrete.braking.CarState
import examples.concrete.braking.Car

/** Sanity tests for Randomized as a Scheduler */
class CarSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  "Sanity checks for symsim.concrete.braking" - {

    // Generators of test data
    val positions = Gen.choose[Double] (0.0, 10.0)
    val velocities = Gen.choose[Double] (0.0,10.0)
    val actions = Gen.oneOf (Car.instances.enumAction.membersAscending)

    // Tests

    "A stopped car cannot move, however much you break" in check {
      forAll (positions, actions) { (p, a) =>
        for (s1, r) <- Car.step (CarState (v = 0.0, p = p)) (a)
        yield s1.p == p
      }
    }

    "The car cannot move backwards by braking" in check {
      forAll (velocities, positions, actions) { (v, p, a) =>
        for (s1, r) <- Car.step (CarState (v, p = p)) (a)
        yield (v != 0 ==> s1.p >= p)
      }
    }

    "Position never becomes negative" in check {
      forAll (velocities, positions, actions) { (v, p, a) =>
        for (s1, r) <- Car.step (CarState (v = v, p = p)) (a)
        yield s1.p >= 0.0
      }
    }


    "Reward is valid 1" in check {
      forAll  (positions, positions, velocities, actions) { (p1, p2, v, a) =>
        for
          (_, r1) <- Car.step (CarState (v = v, p = p1)) (a)
          (_, r2) <- Car.step (CarState (v = v, p = p2)) (a)
        yield p1 <= p2 ==> r1 >= r2
      }
    }


    "Reward is valid 2" in check {
      forAll  (velocities, velocities, positions, actions) { (v1, v2, p, a) =>
        for
          (_, r1) <- Car.step (CarState (v1, p)) (a)
          (_, r2) <- Car.step (CarState (v2, p)) (a)
        yield v1 <= v2 ==> r1 >= r2
      }
    }

  }
