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

/** Sanity tests for symsim.concrete.braking */
class CarSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sanity checks for symsim.concrete.braking" - {

    import Car.instances.{arbitraryState, arbitraryAction}

    // Tests

    "A stopped car cannot move, however much you break" in check {
      forAll { (s: CarState, a: CarAction) =>
        for (s1, r) <- Car.step (s.copy (v = 0.0)) (a)
        yield s1.p == s.p
      }
    }

    "The car cannot move backwards by braking" in check {
      forAll { (s: CarState, a: CarAction) =>
        for (s1, r) <- Car.step (s) (a)
        yield (s.v != 0 ==> s1.p >= s.p)
      }
    }

    "Position never becomes negative" in check {
      forAll { (s: CarState, a: CarAction) =>
        for (s1, r) <- Car.step (s) (a)
        yield s1.p >= 0.0
      }
    }


    "Reward is valid 1" in check {
      forAll  { (s1: CarState, s2: CarState, a: CarAction) => for
        (_, r1) <- Car.step (CarState (v = s1.v, p = s1.p min s2.p)) (a)
        (_, r2) <- Car.step (CarState (v = s1.v, p = s1.p max s2.p)) (a)
      yield r1 >= r2
    } }


    "Reward is valid 2" in check {
      forAll { (s1: CarState, s2: CarState, a: CarAction) => for
        (_, r1) <- Car.step (CarState (v = s1.v min s2.v, p = s1.p)) (a)
        (_, r2) <- Car.step (CarState (v = s1.v max s2.v, p = s1.p)) (a)
      yield r1 >= r2
    } }

  }
