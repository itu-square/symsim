package symsim.examples.concrete.golf

import org.scalacheck.Gen
import org.scalacheck.Prop.*

import symsim.CanTestIn.given
import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized
import symsim.concrete.Randomized.given

// Eliminate the warning on GolfSpec until scalacheck marks Properties it open
import scala.language.adhocExtensions

class GolfSpec
  extends org.scalacheck.Properties ("Golf"):

  import Golf.instances.{arbitraryState, arbitraryAction, enumState, enumAction}

  // The number of episodes set to zero, as it is ignored in tests below
  val sarsa = ConcreteSarsa (Golf, 0.1, 0.1, 0.1, 0)

  import sarsa.vf.apply

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
      for res <- sarsa.learningEpisode ((sarsa.vf.initialize, List[sarsa.vf.Q]()), s)
        q = res._1
      yield q (s, a) <= 0
    }

  property ("Using club D in the sand is the best trained action after 100 episodes") =
    val initials = Randomized.repeat(Randomized.const(Golf.StartState)).take(200)
    val q = sarsa.learn (sarsa.vf.initialize, List[sarsa.vf.Q](), initials)
    q.take(50).forall { (q, _) => sarsa.vf.bestAction (q) (Golf.SandState)._1 == Club.D }
