package symsim
package examples.concrete.mountaincar

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest.*
import org.scalacheck.Prop.{exists, forAll, forAllNoShrink, propBoolean}
import examples.concrete.mountaincar.CarState
import examples.concrete.mountaincar.MountainCar
import Math.cos

/** Sanity tests for Randomized as a Scheduler */
class MountainCarSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sanity checks for symsim.concrete.breaking" - {

    // Generators of test data
    val positions = Gen.choose[Double] (-1.2, 0.5)
    val velocities = Gen.choose[Double] (-1.5,1.5)
    val negPos = Gen.choose[Double] (-1.2, -1.1)
    val actions = Gen.oneOf (MountainCar.instances.enumAction.membersAscending)


    // Tests

    "Achieving goal state from state (0.0, 0.0) is impossible" in check {
      forAll (actions) { a =>
        for (s1, r) <- MountainCar.step (CarState (0.0, 0.0)) (a)
        yield s1.p != 0.5
      }
    }

    "there is a left bound for position" in check {
      val dt: Double = 0.1
      val mass: Double = 0.2
      val friction: Double = 0.3
      val gravity: Double = 9.8
      forAllNoShrink (velocities, negPos, actions) { (v, p, a) =>
        val v1 = v + (gravity * mass * cos(3.0 * p) + a / mass - friction * v) * dt
        val p1 = p + (v1 * dt)
        for (s1, r) <- MountainCar.step (CarState (v, p)) (a)
          yield (p1 < -1.2) ==> (s1.p == -1.2)
      }
    }

    "moving to the left bound of position is possible" in check {
      forAllNoShrink (velocities, actions) { (v, a) =>
        exists (negPos) { p =>
          for (s1, r) <- MountainCar.step (CarState (v, p)) (a)
          yield s1.p == -1.2
        }
      }
    }

  }
