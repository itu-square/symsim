package symsim
package laws

import cats.Eq
import cats.syntax.functor.*

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

import symsim.CanTestIn.*

/** Collect the laws that define correctness of an Agent.  This appears
  * superfluous on top of symsim.law.AgentLaws, but it will become more
  * useful when our agents get more structure, and we get a taxonomy of
  * agents.
  */
case class AgentLaws[State, ObservableState, Action, Reward, Scheduler[_]]
  (agent: Agent[State, ObservableState, Action, Reward, Scheduler])
  extends org.typelevel.discipline.Laws:

  import agent.instances.given

  val finiteStates = agent.instances.enumState.membersAscending

  val laws: RuleSet = SimpleRuleSet (
    "agent",

    /** Law: Every state from the environment/agent state space can
      * be discretized and represented in the enumerable finite state
      * space that the agent uses for learning and for control decisions.
      */
    "discretize (s) ∈ ObservableState" ->
    forAll { (s: State) =>
      finiteStates.contains (agent.discretize (s)) },

    /** Law: Every initialization ends up being discritized to
     *  an enumerable value of ObservableState.
     */
    "discretize (initialize) ∈ ObservableState" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      finiteStates.contains (agent.discretize (s)) },

    /** Law: Initial state is not final, regardless scheduler. */
    "¬isFinal (initialize)" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      !agent.isFinal (s) },

    /** Law: An agent step from any state lands in a state
      * that be discretized into ObservableState.
      */
    "discretize (step (s) (a)._1) ∈ ObservableState" ->
    forAll { (s0: State) =>
      forAll { (a: Action) =>
        val prop = for
          s1r <- agent.step (s0) (a)
          (s1, r) = s1r
          d1 = agent.discretize (s1)
        yield finiteStates contains d1
        prop.toProp
    } },

    /** Law: The initial state is not a fixed point of the step function. So
     *  an exploration is possible from the initial state. 
     */
    "∀ s ∈ initialize ⋅ ∀ a ∈ Actions ⋅ step (s) (a) ≠ (s,_)" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      exists { (a: Action) => 
        val prop = for sr <- agent.step (s) (a) yield sr._1 != s
        prop.toProp
    } }
    // When we add non-episodic tasks this should become: there is at least two
    // actions that have a different reward, or at least one that leads to a
    // new state. Otherwise the test will fail on agents that have a singleton
    // statespace but a reward distribution to learn. It works for our
    // SimpleBandit, because we implemented it as episodic (with two states).
  )
