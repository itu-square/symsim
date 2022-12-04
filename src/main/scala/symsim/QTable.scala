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

  opaque type Q = Map[(ObservableState, Action), Reward]
  type VF = Q

  extension (q: Q) 
    def apply (s: ObservableState, a: Action): Reward = 
      q.getOrElse ((s, a), agent.zeroReward)
    def updated: ((ObservableState, Action), Reward) => Q = q.updated


  /** Construct a zero initialized Q matrix */
  def initialize: Q = Map()

  /** Generate total Q matrices for testing. 
    *
    * Note that just asking for arbitrary[Map[ObservableState, Map[Action,
    * Reward]]] will return random Q-tables, but not guaranteeing that they are
    * total (for instance, by default an empty Map will be produced). 
    **/
  val genVF: Gen[Q] =
    val rewards = List.fill (allActions.size*allObservableStates.size) 
                    (arbitrary[Reward]) 
    val stateAc = for
       s <- allObservableStates
       a <- allActions
    yield (s, a)

    for rs <- Gen.sequence[List[Reward], Reward] (rewards)
    yield Map[(ObservableState, Action), Reward] (stateAc zip rs*)
