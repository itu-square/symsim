package symsim
package examples.concrete.simplebandit

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * Sutton, Barto, Section 2.5 p 32.
 * We map a finite state space with the following
 *
 * We have two states S0 and S1, which S1 is final state.
 *
 * There are 10 actions, which are possible to choose in S1.
 *
 * Reward function is a Gaussian random number at each episode.
 *
 * Optimal policy would be different, according to the considered
 * reward distribution for each action.
 *
 */

type BanditState = Boolean
type BanditFiniteState = BanditState
type BanditReward = Double
type BanditAction = Int


class Bandit (banditReward: List [Randomized[BanditReward]])
  extends Agent[BanditState, BanditFiniteState, BanditAction, BanditReward, Randomized]
  with Episodic:

  val TimeHorizon: Int = 3

    // Bandit has only one final state
    def isFinal (s: BanditState): Boolean =
      s == true

    // Bandit is discrete
    def discretize (s: BanditState): BanditFiniteState =  s
    
    def successor (s: BanditState) (a: BanditAction): BanditState =
         true

    def step (s: BanditState) (a: BanditAction): Randomized[(BanditState, BanditReward)] =
      for
        r <- banditReward (a)
        newState = successor (s) (a)
      yield (newState, r)

    def initialize: Randomized[BanditState] =
      Randomized.oneOf (instances.allFiniteStates.filter { !isFinal (_) }:_*)

    override def zeroReward: BanditReward = 0

    val instances = new BanditInstances(banditReward)

end Bandit

/** Here is a proof that our types actually deliver on everything that an Agent
 * needs to be able to do to work in the framework.
 */
class BanditInstances (banditReward : List [Randomized[BanditReward]])
  extends AgentConstraints[BanditState, BanditFiniteState, BanditAction, BanditReward, Randomized]:

    given enumAction: BoundedEnumerable[BanditAction] = BoundedEnumerableFromList (List.range(1, banditReward.size)*)

    given enumState: BoundedEnumerable[BanditFiniteState] =
    	BoundedEnumerableFromList (false, true)

    given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

    given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

    given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

    lazy val genBanditState: Gen[BanditState] = Gen.oneOf(false, true)

    given arbitraryState: Arbitrary[BanditState] = Arbitrary (genBanditState)

    given eqBanditState: Eq[BanditState] = Eq.fromUniversalEquals

    given arbitraryReward: Arbitrary[BanditReward] = Arbitrary (Gen.double)

    given rewardArith: Arith[BanditReward] = Arith.arithDouble

end BanditInstances
