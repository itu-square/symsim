package symsim
package examples.concrete.cliffwalking

import symsim.concrete.Randomized.given
import CanTestIn.given

import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest.*
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean, exists}
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
    val genA = Gen.choose(0.1, 0.3)
    val genE = Gen.choose(0.001, 0.2)
    val states = Gen.oneOf (CliffWalking.instances.enumState.membersAscending)
    val xs = Gen.oneOf(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
    val ys = Gen.oneOf(Seq(0, 1, 2, 3))
    val actions = Gen.oneOf (CliffWalking.instances.enumAction.membersAscending)

    // Tests

    "All rewards are negative" in check {
      forAll(states, actions) { (s, a) =>
        for (_, r) <- CliffWalking.step (s) (a)
        yield r < 0
      }
    }

//    "The intial state is not terminal" in check {
//      for
//        s <- CliffWalking.initialize
//      yield !CliffWalking.isFinal(s)
//    }


  }
