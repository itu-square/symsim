package symsim
package laws

import cats.Eq
import cats.syntax.functor.*

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

import symsim.CanTestIn.*
import symsim.CanTestIn.given

/** Collect the laws that define correctness of an Agent.  This appears
  * superfluous on top of symsim.law.AgentLaws, but it will become more
  * useful when our agents get more structure, and we get a taxonomy of
  * agents.
  */
case class AgentLaws[State, ObservableState, Action, Reward, Scheduler[_]]
  (agent: Agent[State, ObservableState, Action, Reward, Scheduler])
  extends org.typelevel.discipline.Laws:

  import agent.instances.given

  val observableStates = agent.instances.enumState.membersAscending

  val laws: RuleSet = SimpleRuleSet (
    "agent",

    /** Law: Every state from the environment/agent state space can
      * be observed and represented in the enumerable finite state
      * space that the agent uses for learning and for control decisions.
      */
    "observe (s) ∈ ObservableState" ->
    forAll { (s: State) =>
      observableStates.contains (agent.observe (s)) },

    /** Law: Every initialization ends up being discritized to
     *  an enumerable value of ObservableState.
     */
    "observe (initialize) ∈ ObservableState" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      observableStates.contains (agent.observe (s)) },

    /** Law: Initial state is not final, regardless scheduler. */
    "¬isFinal (initialize)" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      !agent.isFinal (s) },

    /** Law: An agent step from any state lands in a state
      * that be observed into ObservableState.
      */
    "observe (step (s) (a)._1) ∈ ObservableState" ->
    forAll { (s0: State) =>
      forAll { (a: Action) =>
        for
          s1r    <- agent.step (s0) (a)
          (s1, r) = s1r
          d1      = agent.observe (s1)
        yield observableStates.contains (d1)
    } },

    /** Law: The initial state is not a fixed point of the step function. So
     *  an exploration is possible from the initial state. 
     */
    "∀ s ∈ initialize ⋅ ∀ a ∈ Actions ⋅ step (s) (a) ≠ (s,_)" ->
    forAll (agent.initialize.toGen) { (s: State) =>
      exists { (a: Action) => 
        for sr <- agent.step (s) (a) 
        yield sr._1 != s
    } }
    // When we add non-episodic tasks this should become: there is at least two
    // actions that have a different reward, or at least one that leads to a
    // new state. Otherwise the test will fail on agents that have a singleton
    // statespace but a reward distribution to learn. It works for our
    // SimpleBandit, because we implemented it as episodic (with two states).
  )
