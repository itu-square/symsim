package symsim
package laws.discipline

import cats.Eq
import cats.kernel.instances.boolean._
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.language.implicitConversions

import symsim.concrete.Randomized

/** Collect the laws that define correctness of an Agent.  This appears
  * superfluous on top of symsim.law.AgentLaws, but it will become more
  * useful when our agents get more structure, and we get a taxonomy of
  * agents.
  */
class AgentTests[State, FiniteState, Action, Reward, Scheduler[_]]
  extends org.typelevel.discipline.Laws:

  def laws (a: Agent[State, FiniteState, Action, Reward, Scheduler]) =
    new symsim.laws.AgentLaws (a)


  def agent (a: Agent[State, FiniteState, Action, Reward, Scheduler]): RuleSet =
    new SimpleRuleSet (
      "agent",

      "discretize (s) ∈ FiniteState" ->
        laws (a).discretizeIsIntoFiniteState,

      "discretize (initialize) ∈ FiniteState" ->
        laws (a).initializeIsIntoFiniteState,

      "¬isFinal (initialize)" ->
        laws (a).initialStateIsNotFinal,

      "discretize (step (s) (a)._1) ∈ FiniteState" ->
        laws (a).stepIsIntoFiniteState,
    )
