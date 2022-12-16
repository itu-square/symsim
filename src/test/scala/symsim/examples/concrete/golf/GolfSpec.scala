package symsim
package examples.concrete.golf

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.*
import examples.concrete.golf.GolfState
import examples.concrete.golf.Golf
import symsim.concrete.ConcreteSarsa

// To eliminate the warning on GolfSpec, until scalacheck makes it open
import scala.language.adhocExtensions

/** Sanity tests for Randomized as a Scheduler */
class GolfSpec
  extends org.scalacheck.Properties ("Golf"):

    // Generators of test data
    val states = Gen.choose[Int] (1, 10)
    val actions = Gen.oneOf (Golf.instances.allActions)
    val sarsa = ConcreteSarsa(Golf, 0.1, 0.1, 0.1, 100000)


    // Tests

    property ("There is no action that leads agent to initial state") = {
      forAll (states, actions) { (s, a) =>
        for (s1, r) <- Golf.step (s) (a) yield s1 != 1
      }
    }

    property ("Ball stuck in the sand until using club D") = {
      forAll(actions) { a =>
        for (s1, r) <- Golf.step (6) (a)
        yield ((a._1 != Club.D) ==> (s1 == 6))
      }
    }

    property ("Q-table values are non-positive") = {
      forAllNoShrink (states, actions) { (s, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, s)
        yield Q (s) (a) <= 0
      }
    }

    property ("Using club D in the sand is the best") = {
      forAllNoShrink (states, actions) { (s, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, s)
        yield sarsa.bestAction (Q) (6)._1 == Club.D
      }
    }

//    property ("shooting into the sand is not a good choice") = {
//      forAllNoShrink (states, actions) { (s, a) =>
//        for
//          Q <- sarsa.learningEpisode (sarsa.initialize, s)
//        yield (Q (2) ((Club.D, Direction.L)) >= Q (2) ((Club.D, Direction.R))) &&
//                (Q (4) ((Club.P, Direction.L)) >= Q (4) ((Club.P, Direction.R)))
//      }
//    }
