package symsim
package examples.concrete.cliffwalking

import symsim.concrete.Randomized.given
import CanTestIn.given

import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import examples.concrete.cliffwalking.CWState
import examples.concrete.cliffwalking.CliffWalking

/** Sanity tests for Randomized as a Scheduler */
class CliffWalkingSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sanity checks for symsim.concrete.braking" - {

    // Generators of test data
    val states = Gen.oneOf (CliffWalking.instances.enumState.membersAscending)
    val actions = Gen.oneOf (CliffWalking.instances.enumAction.membersAscending)

    // Tests

    "All rewards are negative" in check {
      forAll(states, actions) { (s, a) =>
        for (_, r) <- CliffWalking.step (s) (a)
        yield r < 0
      }
    }

  }
