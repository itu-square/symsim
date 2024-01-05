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
import symsim.concrete.ConcreteExactRL

import scala.util.Try

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
   (sarsa: ExactRL[State, ObservableState, Action, Reward, Scheduler])
   extends org.typelevel.discipline.Laws:

   import sarsa.agent.instances.given
   import sarsa.agent.instances.*
   import sarsa.vf.*

   def isStateTotal (q: sarsa.vf.Q): Boolean =
     q.states.toSet == allObservableStates.toSet

   def isActionTotal (q: sarsa.vf.Q): Boolean =
     q.states.forall { s =>
       q.actionValues (s).keySet == allActions.toSet }

   val laws: RuleSet = new SimpleRuleSet (
      "sarsa",

      /* Law: All values in Q matrix are zero initially */
      "initQ contains only zeroes" -> {
         import sarsa.vf.apply
         val q = sarsa.vf.initialize
         val props = for
           s <- allObservableStates
           a <- allActions
         yield q (s, a) == arith[Reward].zero
         props.forall (identity)
      },

      "generated Q matrices are total for finite state space" ->
      forAll (sarsa.vf.genVF) { (q: sarsa.vf.Q) => isStateTotal (q) },

      "generated Q matrices are total for action state space" ->
      forAll (sarsa.vf.genVF) { (q: sarsa.vf.Q) => isActionTotal (q) },

      /* Law: chooseAction gives one of the enumerable actions */
      "chooseAction (q) (s) ∈ Action for all q and s" ->
      forAll (sarsa.vf.genVF) { (q: sarsa.vf.Q) =>
        forAll { (s: State) =>
          val sa: Scheduler[Action] =
            sarsa.vf.chooseAction (sarsa.ε0) (q) (sarsa.agent.observe (s))
          forAll (sa.toGen) { a => allActions.contains (a)
      } } },
    )
