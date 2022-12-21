package symsim.examples.concrete.golf

import org.scalacheck.Gen
import org.scalacheck.Prop.*

import symsim.CanTestIn.given
import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized.given

// Eliminate the warning on GolfSpec until scalacheck marks Properties it open
import scala.language.adhocExtensions

class GolfSpec
  extends org.scalacheck.Properties ("Golf"):

  import Golf.instances.{arbitraryState, arbitraryAction}

  val sarsa = ConcreteSarsa (Golf, 0.1, 0.1, 0.1, 100000)

  property ("There is no action that leads agent to initial state") =
    forAll { (s: GolfState, a: GolfAction) =>
      for (s1, r) <- Golf.step (s) (a) 
      yield s1 != Golf.StartState
    }

  property ("Ball stuck in the sand until using club D") =
    forAll { (a: GolfAction) =>
      for (s1, r) <- Golf.step (Golf.SandState) (a)
      yield a._1 != Club.D ==> (s1 == Golf.SandState)
    }

  property ("Q-table values are non-positive") =
    forAll { (s: GolfState, a: GolfAction) =>
      for Q <- sarsa.learningEpisode (sarsa.initialize, s)
      yield Q (s) (a) <= 0
    }

  property ("Using club D in the sand is the best trained action") =
    forAll { (s: GolfState, a: GolfAction) =>
      for Q <- sarsa.learningEpisode (sarsa.initialize, s)
      yield sarsa.bestAction (Q) (6)._1 == Club.D
    }
