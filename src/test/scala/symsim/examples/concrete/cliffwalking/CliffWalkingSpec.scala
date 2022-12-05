package symsim
package examples.concrete.cliffWalking

import symsim.concrete.Randomized.given
import CanTestIn.given

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.*
import examples.concrete.cliffWalking.CWState
import examples.concrete.cliffWalking.CliffWalking
import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized

/** Sanity tests for Randomized as a Scheduler */
class CliffWalkingSpec
  extends org.scalacheck.Properties("CliffWalking"):

    // Generators of test data
    val states = Gen.oneOf (CliffWalking.instances.enumState.membersAscending)
    val actions = Gen.oneOf (CliffWalking.instances.enumAction.membersAscending)

    // Tests

    property("All rewards are negative") =
      forAll(states, actions) { (s, a) =>
        for (_, r) <- CliffWalking.step (s) (a)
        yield r < 0
      }
