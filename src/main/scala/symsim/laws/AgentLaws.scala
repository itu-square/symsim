package symsim
package laws

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.kernel.laws._

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

  def discretizeIsIntoFiniteState (s: State): IsEq[Boolean] =
    finiteStates.contains (a.discretize (s)) <-> true

  // TODO: this one is fishy; seed should go out as at this level of abstration
  // we have no idea what is the state of the scheduler.  It appears that we
  // need to know what is the state space of the scheduler, at least abstractly,
  // in order to write laws.  So likely Monad is not enough to know about the
  // scheduler.
  def initializeIsIntoFiniteState (seed: concrete.Seed): IsEq[Scheduler[Boolean]] = {
    val d = for {
          s <- a.initialize
          d =  a.discretize (s)
      } yield finiteStates.contains (d)
    d <-> ms.pure (true)
  }

}
