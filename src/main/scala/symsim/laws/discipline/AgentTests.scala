package symsim
package laws.discipline

import cats.Eq
import cats.kernel.instances.boolean._
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait AgentTests[State, FiniteState, Action, Reward, Scheduler[_]]
  extends org.typelevel.discipline.Laws {

  def laws (a: Agent[State, FiniteState, Action, Reward, Scheduler]) =
    new symsim.laws.AgentLaws (a)

  // TODO this seems far from complete, just one law?
  def agent (a: Agent[State, FiniteState, Action, Reward, Scheduler])
    (implicit arbState: Arbitrary[State], eqState: Eq[State]): RuleSet =
    new SimpleRuleSet (
      "agent",
      "discretize (s) ∈ FiniteState" ->
        forAll (laws (a).discretizeIsIntoFiniteState _)
    )
}

object AgentTests {
  def apply[State, FiniteState, Action, Reward, Scheduler[_]]
    : AgentTests[State, FiniteState, Action, Reward, Scheduler] =
    new AgentTests[State, FiniteState, Action, Reward, Scheduler] { }
}
