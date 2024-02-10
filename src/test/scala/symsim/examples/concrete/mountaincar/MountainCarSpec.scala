package symsim
package examples.concrete.mountaincar

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalatest.*
import prop.*
import org.scalacheck.Gen
import org.scalacheck.Prop.{exists, forAll, forAllNoShrink, propBoolean}
import examples.concrete.mountaincar.CarState
import examples.concrete.mountaincar.MountainCar
import Math.cos

// To eliminate the warning on MountainCarSpec, until scalacheck makes it open
import scala.language.adhocExtensions

/** Sanity tests for Randomized as a Scheduler */
class MountainCarSpec
  extends org.scalacheck.Properties ("mountaincar.Car"):

  // Generators of test data
  val positions = Gen.choose[Double] (-1.2, 0.5)
  val velocities = Gen.choose[Double] (-1.5,1.5)
  val negPos = Gen.choose[Double] (-1.2, -1.1)
  val actions = Gen.oneOf (MountainCar.instances.enumAction.membersAscending)


  // Tests

  property ("Achieving the goal state from state (0.0, 0.0) with a single action is impossible") =
    forAll (actions) { a =>
      for (s1, r) <- MountainCar.step (CarState (0.0, 0.0)) (a)
      yield s1.p < 0.5
    }

  property ("The car cannot move beyond -1.2 on the left") =
    val dt: Double = 0.1
    val mass: Double = 0.2
    val friction: Double = 0.3
    val gravity: Double = 9.8
    forAll (velocities, negPos, actions) { (v, p, a) =>
      val v1 = v + (-gravity * mass * cos(3.0 * p) + a / mass - friction * v) * dt
      val p1 = p + (v1 * dt)
      for (s1, r) <- MountainCar.step (CarState (v, p)) (a)
        yield (p1 < -1.2) ==> (s1.p == -1.2)
    }

  property ("The car can move to the position -1.2") =
    val (s1, r) = MountainCar.step (
      CarState (-1.1601645563843357, -1.1533417842134712)) (-0.2).head
    s1.p == -1.2

  property ("gravity and friction are both decreasing the velocity") =
    forAllNoShrink (velocities, negPos) { (v, p1) =>
      for
        (s1, r1) <- MountainCar.step(CarState(v, p1))(0)
        (s2, r2) <- MountainCar.step(CarState(v, s1.p))(0)
      yield ((p1 < s1.p) ==> (s1.v < s2.v)) || ((p1 > s1.p) ==> (s1.v > s2.v))
    }
