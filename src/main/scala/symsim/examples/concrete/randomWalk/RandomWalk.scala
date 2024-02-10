package symsim
package examples.concrete.randomWalk

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * Sutton & Barto, Example 6.2, p. 125
 * We made some changes in the dynamics of original Random Walk from the book, as following
 * State space is continuous, and observable states are every unit discretized states.
 * Actions are +1 and -1 but with a random noise between (-0.25, 0.25) that makes the
 * size of walks shorter or longer.
 * There are two final cases, which one is for winning and the other is loosing.
 * Reward for winning and loosing is +1, -1 respectively, -0.1 otherwise.
 */

type RandomWalkState = Double

type RandomWalkObservableState = Int
type RandomWalkReward = Double

type RandomWalkAction = Double

val LeftWall: Double = -100.0
val RightWall: Double = 100.0

object RandomWalk
  extends 
    Agent[RandomWalkState, RandomWalkObservableState, RandomWalkAction, RandomWalkReward, Randomized],
    Episodic:

  val TimeHorizon: Int = 100

  def isFinal (s: RandomWalkState): Boolean =
    (s <= 5.0 && s >= 4.0) || (s <= -4.0 && s >= -5.0)

  def observe (s: RandomWalkState): RandomWalkObservableState =
//    ((s/2.0).floor * 2.0).toInt
    s.floor.toInt

  private def randomWalkReward (s: RandomWalkState): RandomWalkReward =
    if s <= 5.0 && s >= 4.0   then  1.0  // Good final state
    if s <= -4.0 && s >= -5.0 then -1.0 // Bad final state (dead)
    else -0.01

  val attention = 0.8

  def valid (s: RandomWalkState): Boolean =
    s <= RightWall && s >= LeftWall

  def step (s: RandomWalkState) (a: RandomWalkAction):
  Randomized[(RandomWalkState, RandomWalkReward)] =
    require (valid (s))
    for
      noise <- Randomized.between(-0.25, 0.25)
      newState = if valid (s + a + noise) then s + a + noise else s
  //    _ = print(newState)
    yield (newState, randomWalkReward (newState))

  def initialize: Randomized[RandomWalkState] = for
    s <- Randomized.between (LeftWall, RightWall)
  yield s

  val instances = RandomWalkInstances

end RandomWalk


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object RandomWalkInstances
   extends AgentConstraints[RandomWalkState, RandomWalkObservableState, RandomWalkAction, RandomWalkReward, Randomized]:

   given enumAction: BoundedEnumerable[RandomWalkAction] =
      BoundedEnumerableFromList (-1.0, 1.0)

   given enumState: BoundedEnumerable[RandomWalkObservableState] =
      val ss = for
        s <- List.range(LeftWall.toInt, RightWall.toInt, 1)
      yield s
      BoundedEnumerableFromList (ss*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genRandomWalkState: Gen[RandomWalkState] = for
      s <- Gen.choose (LeftWall, RightWall)
   yield (s)

   given arbitraryState: Arbitrary[RandomWalkState] = Arbitrary (genRandomWalkState)

   given eqRandomWalkState: Eq[RandomWalkState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[RandomWalkReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[RandomWalkReward] = Arith.arithDouble

end RandomWalkInstances
