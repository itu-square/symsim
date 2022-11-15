package symsim
package examples.concrete.golf

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalatest.*
import prop.*
import org.scalacheck.Gen
import org.scalacheck.Prop.{exists, forAll, forAllNoShrink, propBoolean}
import examples.concrete.golf.GolfState
import examples.concrete.golf.Golf
import symsim.concrete.ConcreteSarsa

/** Sanity tests for Randomized as a Scheduler */
class GolfSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sanity checks for symsim.concrete.golf" - {

    // Generators of test data
    val states = Gen.choose[Int] (1, 10)
    val actions = Gen.oneOf (Golf.instances.allActions)
    val sarsa = ConcreteSarsa(Golf, 0.1, 0.1, 0.1, 100000)


    // Tests

    "There is no action that leads agent to initial state" in check {
      forAll (states, actions) { (s, a) =>
        for (s1, r) <- Golf.step (s) (a) yield s1 != 1
      }
    }

    "Ball stuck in the sand until using club D" in check {
      forAll(actions) { a =>
        for (s1, r) <- Golf.step (6) (a)
        yield ((a._1 != Club.D) ==> (s1 == 6))
      }
    }

    "Q-table values are non-positive" in check {
      forAllNoShrink (states, actions) { (s, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, s)
        yield Q (s) (a) <= 0
      }
    }

    "Using club D in the sand is the best" in check {
      forAllNoShrink (states, actions) { (s, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, s)
        yield sarsa.bestAction (Q) (6)._1 == Club.D
      }
    }

    "shooting into the sand is not a good choice" in check {
      forAllNoShrink (states, actions) { (s, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, s)
        yield (Q (2) ((Club.D, Direction.L)) >= Q (2) ((Club.D, Direction.R))) &&
                (Q (4) ((Club.P, Direction.L)) >= Q (4) ((Club.P, Direction.R)))
      }
    }

  }
