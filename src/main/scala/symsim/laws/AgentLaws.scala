package symsim
package laws

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.kernel.laws._

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary

import symsim._

/**
 * Laws that have to be obeyed by any refinement of symsim.Agent
 *
 * TODO: So many  of the type paramaters are a hell of an annoyance.  So we need
 * to do something about this.  Also the fact that we have a class,  and other
 * designs that I have seen do not seem to need the value of the tested object,
 * the design of tests still should be investigated.  But we have something that
 * can be used to run tests for now.
 */
class AgentLaws[State, FiniteState, Action, Reward, Scheduler[_]] (
  val a: Agent[State, FiniteState, Action, Reward, Scheduler]
){
  implicit val ms: Monad[Scheduler] = a.schedulerIsMonad

  val finiteStates = a.enumState.membersAscending

  /** Law: every state from the environment/agent state space
    * can be discretized and represented in the enumerable finite state
    * space that the agent uses for learning and for control decisions.
    */
  def discretizeIsIntoFiniteState (implicit as: Arbitrary[State]) =
    forAll { s: State => finiteStates.contains (a.discretize (s)) }

  /** Law: every initialization ends up being discritized to an enumerable value
    * of FiniteState.
    */
  def initializeIsIntoFiniteState
    (implicit checkInScheduler: Scheduler[Boolean] => Prop): Prop = {

      val scheduledProp = for {
            s <- a.initialize
            d =  a.discretize (s)
      } yield finiteStates.contains (d)

      checkInScheduler (scheduledProp)
  }

}
