package symsim
package laws

import cats.kernel.laws._
import cats.kernel.laws.discipline._
import org.scalacheck.Prop
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty._
import cats.kernel.BoundedEnumerable

/**
 * Laws that have to be obeyed by any refinement of symsim.SARSA
 *
 * TODO: So many  of the type paramaters are a hell of an annoyance.  So we need
 * to do something about this.  Also the fact that we have a class,  and other
 * designs that I have seen do not seem to need the value of the tested object,
 * the design of tests still should be investigated.  But we have something that
 * can be used to run tests for now.
 *
 * Same comment in AgentLaws.scala
 */
class SarsaLaws[State,FiniteState, Action, Reward, Scheduler[_]] (
  val s: Sarsa[State, FiniteState, Action, Reward, Scheduler]
) {

  import s.agent.instances._

  type S = Sarsa[State, FiniteState, Action, Reward, Scheduler]
  type A = S#A

  // TODO: If learn1 starts with a positive values, then we end with positive
  // values

  // TODO: If learn1 produces a larger (pointwise matrix) than Q

  // TODO: domain of Q is invariant in learn1

  // TODO: learnN (1) is the same as learn

  // TODO: best_action and choose_action always return actions that can be
  // discretized to FiniteState elem (or are in Action elem)

  // TODO qToPolicy preserves that state and action domains of Q

  // TODO initQ creates a matrix of the right size that is total and
  // initialized to zeroes

  // TODO  "this is for RL: eventually we arrive at final (episodic agent)" ->
  //      Prop.falsified,

    /** Law: Q matrix has a action-reward map for each finite state */
    def initQDefinedForAllFiniteStates: Prop =
      s.initQ.keySet <-> BoundedEnumerable[FiniteState].membersAscending.toSet

    /** Check that the initialization of Q matrix is correct */
    def initQDefinedForAllActions: Prop = {

      val inType = BoundedEnumerable[Action].membersAscending.toSet
      val inQ: Iterable[Set[Action]] = s.initQ
        .values
        .map[Set[Action]] { _.keySet }

      inQ.forall { _ == inType }
    }

    /** Law: All values in Q matrix are zeroReward initially */
    def initQAllValuesZero: Prop = {

      val props =  for {
        vector <- s.initQ.values
        cell   <- vector.values
      } yield cell == s.agent.zeroReward

      props.forall (identity)
    }

}
