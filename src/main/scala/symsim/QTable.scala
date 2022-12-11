package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.typelevel.paiges.Doc
import symsim.concrete.{ConcreteExactRL, Randomized}


trait QTable[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ValueFunction[ObservableState, Action, Reward, Scheduler]:
  this: ExactRL[State, ObservableState, Action, Reward, Scheduler] =>

  import agent.instances.*
  import agent.instances.given

  opaque type Q = Map[ObservableState, Map[Action, Reward]]
  type VF = Q

  extension (q: Q) 
    def apply (s: ObservableState, a: Action): Reward = 
      q.getOrElse (s, Map[Action, Reward]()).getOrElse (a, agent.zeroReward)
    def updated (s: ObservableState, a: Action, v: Reward): Q = 
      q.updated (s, (q.getOrElse (s, Map ()).updated (a, v))) 
    private /* TODO? */ def actionValues (s: ObservableState): Map[Action, Reward] = q (s)


  /** Construct a zero initialized Q matrix */
  def initialize: Q = Map ()

  /** Generate total Q matrices for testing. 
    *
    * Note that just asking for arbitrary[Map[ObservableState, Map[Action,
    * Reward]]] will return random Q-tables, but not guaranteeing that they are
    * total (for instance, by default an empty Map will be produced). 
    **/
  val genVF: Gen[Q] =
    val l1 = List.fill (allActions.size) (arbitrary[Reward]) 
    val genActionRewardMap = 
      for rs <- Gen.sequence[List[Reward], Reward] (l1)
      yield Map (allActions zip rs*)

    val  l2 = List.fill (allObservableStates.size) (genActionRewardMap)
    for maps <- Gen.sequence[List[Map[Action, Reward]], Map[Action, Reward]] (l2)
    yield Map (allObservableStates zip maps*)
