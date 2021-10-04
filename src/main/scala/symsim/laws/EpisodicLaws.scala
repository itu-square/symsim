package symsim
package laws

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.kernel.laws._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAllNoShrink

import symsim.concrete._
import symsim.CanTestIn._


/** Laws that have to be obeyed by a refinement of symsim.Agent that is Episodic
  * (Does not include laws of the Agent itself)
  */
class EpisodicLaws[State, FiniteState, Action, Reward, Scheduler[_] ]
  (val agent: Agent[State, FiniteState, Action, Reward, Scheduler] with Episodic):

  import agent.instances.given

  /** for every initial state s
    *    for TimeHorizon steps,
    *       pick up a random action
    *       take step under this action
    *    reach the final state before TimeHorizon
    */
   def everyEpisodeTerminates: Prop =

     val genActStream: Gen[LazyList[Action]] =
        Gen.containerOfN[LazyList,Action] (agent.TimeHorizon, arbitrary[Action])

     forAll (agent.initialize.toGen) { s =>
        forAllNoShrink (genActStream) { (la: LazyList[Action]) =>
           summon[Monad[Scheduler]]
              .iterateUntilM[(LazyList[Action], State)] (la -> s)
                 { case (a #:: tl, s) => agent.step (s) (a)
                                              .map { case (s,r) => (tl,s) }  }
                 { case (la,s) => la.isEmpty || agent.isFinal (s) }
              .map { case (la,s) => la.nonEmpty }
              .toProp
        }
     }
