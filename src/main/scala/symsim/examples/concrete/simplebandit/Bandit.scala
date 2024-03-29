package symsim
package examples.concrete.simplebandit

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * Sutton, Barto, Section 2.5 p 32.
 *
 * We have two states false and true, which true is the final state.
 *
 * There are 10 actions, which are possible to choose in false state.
 * 
 * We consider two types of experiments based on different Reward functions.
 * First, a random number using Cons distribution at each episode.
 * Second, a random number using Gaussian distribution at each episode.
 *
 * Optimal policy would be different, according to the considered
 * reward distribution for each action.
 *
 */

type BanditState = Boolean
type BanditReward = Double
type BanditAction = Int


class Bandit (banditReward: List [Randomized[BanditReward]])
  extends Agent[BanditState, BanditState, BanditAction, BanditReward, Randomized]
  with Episodic:

  val TimeHorizon: Int = 3

  // Bandit has only one final state
  override def isFinal (s: BanditState): Boolean = s

  // Bandit is discrete
  override def observe (s: BanditState): BanditState = s
  
  override def step (s: BanditState) (a: BanditAction): Randomized[(BanditState, BanditReward)] =
    for r <- banditReward (a) yield (true, r)

  override def initialize: Randomized[BanditState] =
    Randomized.const (false)

  override val instances = BanditInstances (banditReward)

end Bandit

/** Here is a proof that our types actually deliver on everything that an Agent
 * needs to be able to do to work in the framework.
 */
class BanditInstances (banditReward: List [Randomized[BanditReward]])
  extends AgentConstraints[BanditState, BanditState, BanditAction, BanditReward, Randomized]:

    given enumAction: BoundedEnumerable[BanditAction] = 
      BoundedEnumerableFromList (List.range(0, banditReward.size)*)

    given enumState: BoundedEnumerable[BanditState] =
    	BoundedEnumerableFromList (false, true)

    given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

    given schedulerIsFoldable: Foldable[Randomized] = 
      Randomized.randomizedIsFoldable

    given canTestInScheduler: CanTestIn[Randomized] = 
      Randomized.canTestInRandomized

    lazy val genBanditState: Gen[BanditState] = Gen.oneOf (false, true)

    given arbitraryState: Arbitrary[BanditState] = Arbitrary (genBanditState)

    given eqBanditState: Eq[BanditState] = Eq.fromUniversalEquals

    given arbitraryReward: Arbitrary[BanditReward] = Arbitrary (Gen.double)

    given rewardArith: Arith[BanditReward] = Arith.arithDouble

end BanditInstances
