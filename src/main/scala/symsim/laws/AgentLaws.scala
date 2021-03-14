package symsim
package laws

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.kernel.laws._

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary

/**
 * Laws that have to be obeyed by any refinement of symsim.Agent
 *
 * TODO: So many  of the type paramaters are a hell of an annoyance.  So we need
 * to do something about this.  Also the fact that we have a class,  and other
 * designs that I have seen do not seem to need the value of the tested object,
 * the design of tests still should be investigated.  But we have something that
 * can be used to run tests for now.
 */
class AgentLaws[State, FiniteState, Action, Reward, Scheduler[_] ]
  (val agent: Agent[State, FiniteState, Action, Reward, Scheduler])
{

  import agent.instances._
  import symsim.concrete._
  import symsim.CanTestIn._

  val finiteStates = enumState.membersAscending

  /** Law: Every state from the environment/agent state space
    * can be discretized and represented in the enumerable finite state
    * space that the agent uses for learning and for control decisions.
    */
  def discretizeIsIntoFiniteState: Prop =
    forAll { s: State =>
      finiteStates.contains (agent.discretize (s)) }


  /** Law: Every initialization ends up being discritized to an enumerable value
    * of FiniteState.
    */
  def initializeIsIntoFiniteState: Prop = {
    val ini = agent.initialize
    forAll (ini.toGen) { s: State =>
      finiteStates.contains (agent.discretize (s)) }
  }


  /** Law: Initial state is not final, regardless scheduler. */
  def initialStateIsNotFinal: Prop = {
    val ini = agent.initialize
    forAll (ini.toGen) { s: State =>
      ! agent.isFinal (s) }
  }


  /** Law: An agent step from any state lands in a state that be discretized
    * into FiniteState.
    */
  def stepIsIntoFiniteState: Prop =
    forAll { s0: State =>
      forAll { a: Action =>
        val (s1, r) = agent.step (s0) (a)
        val d1 = agent.discretize (s1)
        finiteStates.contains (d1)
    } }

}
