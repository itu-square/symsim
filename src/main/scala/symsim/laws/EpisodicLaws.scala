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

import symsim.concrete._
import symsim.CanTestIn._


/** Laws that have to be obeyed by a refinement of symsim.Agent that is Episodic
  * (Does not include laws of the Agent itself)
  */
class EpisodicLaws[State, FiniteState, Action, Reward, Scheduler[_] ]
  (val agent: Agent[State, FiniteState, Action, Reward, Scheduler] with Episodic):

  import agent.instances.given

  /** for every initial state s
    *       for Timeout steps,
    *           pick up a random policy p
    *           pick action according to p and apply step
    *       exit if reached final state
    *       fail if not reached final state until Timeout.
    */
   def everyEpisodeTerminates: Prop =

     given genActStream: Gen[LazyList[Action]] =
        Gen.containerOfN[LazyList,Action](agent.TimeHorizon: Int, arbitrary[Action])

     //def terminatesIn (n: Int) (s: State): Prop =
       //n match {
         //0 => true
         //n =>
       //}

     forAll (agent.initialize.toGen) { s =>
        forAll { (a: LazyList[Action]) =>
           a.isEmpty } }
