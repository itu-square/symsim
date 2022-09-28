package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import org.scalacheck.Gen
import org.typelevel.paiges.Doc
import symsim.concrete.{ConcreteExactRL, Randomized}


trait QTable[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ValueFunction[State, ObservableState, Action, Reward, Scheduler]:
  this: ExactRL[State, ObservableState, Action, Reward, Scheduler] =>
  import agent.instances.*

  type Q = Map[ObservableState, Map[Action, Reward]]
  type VF = Q


  /** Construct a zero initialized Q matrix */
  def initialize: Q =
    // Create the initial Q matrix (zero's everywhere)
    val qa = allActions
      .map { a => (a, agent.zeroReward) }
      .toMap

    val q0 = allObservableStates
      .map { state => (state, qa) }
      .toMap

    q0


  /** Generate total Q matrices for testing. */
  val genVF: Gen[Q] =
    val as = agent.instances.allActions
    val genReward = agent.instances.arbitraryReward.arbitrary
    val genActionReward: Gen[Map[Action, Reward]] = for
      rewards <- Gen.sequence[List[Reward], Reward] {
        List.fill (as.size) (genReward) }
      ars = as zip rewards
    yield Map (ars: _*)

    val fs = agent.instances.allObservableStates
    val genStateActionRewards: Gen[Q] = for
      mars <- Gen.sequence[List[Map[Action, Reward]],
        Map[Action, Reward]] { List.fill (fs.size) (genActionReward) }
      smars = fs zip mars
    yield Map (smars*)

    genStateActionRewards
