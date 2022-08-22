package symsim
package laws

import cats.Eq
import cats.Monad
import cats.syntax.functor.*
import cats.kernel.instances.boolean.*

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAllNoShrink

import symsim.concrete.Randomized
import symsim.CanTestIn.*

/** Collect the laws that define correctness of an Agent with Episodic.
  **/
case class EpisodicLaws[State, ObservableState, Action, Reward, Scheduler[_]]
   (agent: Agent[State, ObservableState, Action, Reward, Scheduler] with Episodic)
   extends org.typelevel.discipline.Laws:

   import agent.instances.given

   val genActStream: Gen[LazyList[Action]] =
      Gen.containerOfN[LazyList, Action] (agent.TimeHorizon, arbitrary[Action])

   val laws: RuleSet = new SimpleRuleSet (
      "episodic agent",

      /** for every initial state s
        *    for TimeHorizon steps,
        *       pick up a random action
        *       take step under this action
        *    reach the final state before TimeHorizon
        */
      "terminates before TimeHorizon" ->
      forAll (agent.initialize.toGen) { s =>
         forAllNoShrink (genActStream) { (la: LazyList[Action]) =>
            summon[Monad[Scheduler]]
               .iterateUntilM[(LazyList[Action], State)] (la -> s)
                  { case (a #:: tl, s) => agent.step (s) (a)
                                               .map { (s, r) => (tl, s) } }
                  { (la, s) => la.isEmpty || agent.isFinal (s) }
               .map { (la, s) => la.nonEmpty }
               .toProp
         }
      },
   )
