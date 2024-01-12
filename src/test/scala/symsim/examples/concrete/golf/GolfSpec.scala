package symsim.examples.concrete.golf

import cats.syntax.all.* 

import org.scalacheck.Gen
import org.scalacheck.Prop.*

import symsim.CanTestIn.given
import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized

// Eliminate the warning on GolfSpec until scalacheck marks Properties it open
import scala.language.adhocExtensions

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val golf = new Golf 

class GolfSpec
  extends org.scalacheck.Properties ("Golf"):

  import golf.instances.{enumAction, enumState, arbitraryState, arbitraryAction}

  // The number of episodes set to zero, as it is ignored in tests below
  val sarsa = ConcreteSarsa (golf, 0.1, 0.1, 0.1, 0)

  import sarsa.vf.apply

  property ("There is no action that leads agent to initial state") =
    forAll { (s: GolfState, a: GolfAction) =>
      for (s1, r) <- golf.step (s) (a) 
      yield s1 != golf.StartState
    }

  property ("Ball stuck in the sand until using club D") =
    forAll { (a: GolfAction) =>
      for (s1, r) <- golf.step (golf.SandState) (a)
      yield a._1 != Club.D ==> (s1 == golf.SandState)
    }

  property ("Q-table values are non-positive") =
    forAll { (s: GolfState, a: GolfAction) =>
      for res <- sarsa.learningEpisode ((sarsa.vf.initialize, List[sarsa.vf.Q](), sarsa.ε0), s)
        q = res._1
      yield q (s, a) <= 0
    }

  property ("Using club D in the sand is the best trained action after 100 episodes") =
    val initials = Randomized.repeat(Randomized.const(golf.StartState)).take(200)
    val q = sarsa.learn (sarsa.vf.initialize, List[sarsa.vf.Q](), initials)
    q.sample (50)
     .forall { (q, _) => sarsa.vf.bestAction (q) (golf.SandState)._1 == Club.D }
