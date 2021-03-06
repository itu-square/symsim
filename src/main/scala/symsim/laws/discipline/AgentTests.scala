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
trait AgentTests[State, FiniteState, Action, Reward, Scheduler[_]]
  extends org.typelevel.discipline.Laws {


  implicit def propInScheduler: Scheduler[Boolean] => Prop


  def laws (a: Agent[State, FiniteState, Action, Reward, Scheduler]) =
    new symsim.laws.AgentLaws (a)


  def agent (a: Agent[State, FiniteState, Action, Reward, Scheduler])
    (implicit arbState: Arbitrary[State], eqState: Eq[State]): RuleSet =
    new SimpleRuleSet (
      "agent",
      "discretize (s) ∈ FiniteState" ->
        laws (a).discretizeIsIntoFiniteState,
      "discretize (initialize) ∈ FiniteState" ->
        laws (a).initializeIsIntoFiniteState,
        // quantify over schedulers: laws (a).initializeIsIntoFiniteState,
      "* ! (initialize.isFinal)" ->
        false,
      "discretize (step (s) (a)._1) ∈ FiniteState" ->
        false
    )

}

object AgentTests {
  def apply[State, FiniteState, Action, Reward, Scheduler[_]]
  (implicit lift: Scheduler[Boolean] => Prop)
    : AgentTests[State, FiniteState, Action, Reward, Scheduler] =

    new AgentTests[State, FiniteState, Action, Reward, Scheduler] {
      implicit val propInScheduler = lift
    }

}
