package symsim
package laws.discipline

import cats.Eq
import cats.kernel.instances.boolean._
import cats.laws.discipline._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import symsim.concrete.Randomized

/** Collect the laws that define correctness of an Agent with Episodic.
  **/
class EpisodicTests[State, FiniteState, Action, Reward, Scheduler[_]]
   extends org.typelevel.discipline.Laws:

   def laws (a: Agent[State, FiniteState, Action, Reward, Scheduler] with Episodic) =
      new symsim.laws.EpisodicLaws (a)

   def agent (a: Agent[State, FiniteState, Action, Reward, Scheduler] with Episodic): RuleSet =
      new SimpleRuleSet (
        "episodic agent",

        "terminates before TimeHorizon" ->
        laws (a).everyEpisodeTerminates,
      )
