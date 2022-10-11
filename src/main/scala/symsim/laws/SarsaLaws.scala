package symsim
package laws

import cats.kernel.laws.*
import cats.kernel.laws.discipline.*
import cats.kernel.BoundedEnumerable

import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.*

import symsim.CanTestIn.*
import symsim.Arith.*

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
case class SarsaLaws[State, ObservableState, Action, Reward, Scheduler[_]]
   (sarsa: Sarsa[State, ObservableState, Action, Reward, Scheduler])
   extends org.typelevel.discipline.Laws:

   import sarsa.agent.instances.given

   def isStateTotal (q: sarsa.Q): Boolean =
     q.keySet == sarsa.agent.instances.allObservableStates.toSet

   def isActionTotal (q: sarsa.Q): Boolean =
     q.values.forall { _.keySet == sarsa.agent.instances.allActions.toSet }

   val laws: RuleSet = new SimpleRuleSet (
      "sarsa",

      /* Law: Q matrix has a action-reward map for each finite state */
      "initQ defined for all ObservableStates" -> isStateTotal (sarsa.initialize),

      /* Law: Check that the initialization of Q matrix is correct */
      "initQ defined for all Actions for each source state" ->
      isActionTotal (sarsa.initialize),

      /* Law: All values in Q matrix are zeroReward initially */
      "initQ contains only zeroRewards" -> {
         val props = for
           vector <- sarsa.initialize.values
           cell   <- vector.values
         yield cell == sarsa.agent.zeroReward
         props.forall (identity)
      },

      "generated Q matrices are total for finite state space" ->
      forAll (sarsa.genVF) { (q: sarsa.Q) => isStateTotal (q) },

      "generated Q matrices are total for action state space" ->
      forAll (sarsa.genVF) { (q: sarsa.Q) => isActionTotal (q) },

      /* Law: chooseAction gives one of the enumerable actions */
      "chooseAction (q) (s) ∈ Action for all q and s" ->
      forAll (sarsa.genVF) { (q: sarsa.Q) =>
        forAll { (s: State) =>
          val sa: Scheduler[Action] =
            sarsa.chooseAction (q) (sarsa.agent.discretize (s))
          forAll (sa.toGen) { a =>
            sarsa.agent.instances.allActions.contains (a)
      } } },
    )
