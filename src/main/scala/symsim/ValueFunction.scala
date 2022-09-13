package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import org.scalacheck.Gen
import org.typelevel.paiges.Doc
import symsim.concrete.{ConcreteExactRL, Randomized}


trait ValueFunction[State, ObservableState, Action, Reward, Scheduler[_]]:
   type VF
   def bestAction (vf: VF) (s: State): Action
   def chooseAction (vf: VF) (s: State): Scheduler[Action]
   def initialize: VF


trait QTable[State, ObservableState, Action, Reward, Scheduler[_]]
   extends ValueFunction[State, ObservableState, Action, Reward, Scheduler]:
   this: ExactRL[State, ObservableState, Action, Reward, Scheduler] =>
   import agent.instances.*

   type VF = Map[ObservableState, Map[Action, Reward]]
   type Q = VF


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
   val genQ: Gen[Q] =
      val as = agent.instances.allActions
      val genReward = agent.instances.arbitraryReward.arbitrary
      val genActionReward: Gen[Map[Action, Reward]] = for
      // TODO refactor, seek what is available for maps
         rewards <- Gen.sequence[List[Reward], Reward] { List.fill (as.size) (genReward) }
         ars = as zip rewards
      yield Map (ars: _*)

      val fs = agent.instances.allObservableStates
      val genStateActionRewards: Gen[Q] = for
      // TODO refactor, seek what is available for maps
         mars <- Gen.sequence[List[Map[Action, Reward]], Map[Action, Reward]] { List.fill (fs.size) (genActionReward) }
         smars = fs zip mars
      yield Map (smars*)

      genStateActionRewards

