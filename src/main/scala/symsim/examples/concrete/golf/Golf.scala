package symsim
package examples.concrete.golf

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * Russell, Norvig, Example 3.6 p. 61 [83] and the following pages.
 * We map a finite state space with the following golf states
 *
 *   { |1|, |2|, |3|, |4|, |5|, |7, 8|, |9|, |10|}.
 *                         |6|
 * which 5 and 6 are in the same area but 6 is sand.
 * Also, 7 and 8 are both the green area but 8 is closer to the goal.
 * 1 is initial state, 9 is goal state, and 10 is area of over shooting.
 *
 * Action set is a tuple of type of club and the shoot direction.
 *
 * Transition is deterministic.
 *
 *
 */

type GolfState = Int
type GolfObservableState = GolfState
type GolfReward = Double

enum Club:
    case P, D
enum Direction:
    case R, L
type GolfAction = (Club, Direction)

object Golf
   extends Agent[GolfState, GolfObservableState, GolfAction, GolfReward, Randomized]
   with Episodic:

      val TimeHorizon: Int = 2000

      def isFinal (s: GolfState): Boolean =
         s == 9

      // Golf is discrete
      def discretize (s: GolfState): GolfObservableState =  s

      private def golfReward (s: GolfState) (a: GolfAction): GolfReward = (s, a) match
         case (6, (Club.P, _)) => -100.0
         case (6, (Club.D, _)) => -2.0
         case (8, _) => -1.0
         case (10, _) => -1.0
         case (5, _) => -2.0
         case (7, (Club.P, _)) => -1.0
         case (7, (Club.D, _)) => -2.0
         case (i, (Club.P, _)) => -7.0+i
         case (4, (Club.D, _)) => -2.0
         case (_, (Club.D, _)) => -3.0

      def successor (s: GolfState) (a: GolfAction): GolfState =
         require (valid (s))
         val result = (s, a) match
            case (10, _) => 9
            case (8, _) => 9
            case (7, (Club.P, _)) => 9
            case (7, (Club.D, _)) => 10
            case (6, (Club.P, _)) => 6
            case (6, (Club.D, _)) => 7
            case (5, (Club.P, _)) => 7
            case (5, (Club.D, _)) => 8
            case (4, (Club.P, Direction.L)) => 5
            case (4, (Club.P, Direction.R)) => 6
            case (4, (Club.D, _)) => 8
            case (i, (Club.P, _)) => i+1
            case (3, (Club.D, _)) => 7
            case (2, (Club.D, Direction.L)) => 5
            case (2, (Club.D, Direction.R)) => 6
            case (1, (Club.D, _)) => 4
            case (_, (_, _)) => 9
         if valid (result) then result else s

      def valid (s: GolfState): Boolean =
         s >= 1 && s <= 10

      def step (s: GolfState) (a: GolfAction): Randomized[(GolfState, GolfReward)] =
         for
            action <- Randomized.const (a)
            newState = successor (s) (action)
         yield (newState, golfReward (s) (action))

      def initialize: Randomized[GolfState] =
         Randomized.const (1)

      override def zeroReward: GolfReward = 0

      val instances = GolfInstances

end Golf


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object GolfInstances
   extends AgentConstraints[GolfState, GolfObservableState, GolfAction, GolfReward, Randomized]:

   given enumAction: BoundedEnumerable[GolfAction] =
      BoundedEnumerableFromList ((Club.P, Direction.L), (Club.P, Direction.R), (Club.D, Direction.L), (Club.D, Direction.R))

   given enumState: BoundedEnumerable[GolfObservableState] =
      BoundedEnumerableFromList (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genGolfState: Gen[GolfState] = Gen.choose (1, 10)

   given arbitraryState: Arbitrary[GolfState] = Arbitrary (genGolfState)

   given eqGolfState: Eq[GolfState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[GolfReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[GolfReward] = Arith.arithDouble

end GolfInstances
