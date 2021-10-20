package symsim
package examples.concrete.simplemaze

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * Russell, Norvig, Fig 17.1, p. 646
 * We map a finite state space with the following maze states
 *
 *   { (1,3), (2,3), (3,3), (4,3),
 *     (1,2),        (3,2), (4,2),
 *     (1,1), (2,1), (3,1), (4,1) } .
 *
 */
case class MazeState (x: Int, y: Int):
   override def toString: String = s"($x,$y)"


type MazeFiniteState = MazeState
type MazeReward = Double

sealed trait MazeAction
case object Left extends MazeAction
case object Right extends MazeAction
case object Up extends MazeAction
case object Down extends MazeAction


object Maze
   extends Agent[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized]:

      def isFinal (s: MazeState): Boolean =
         s == MazeState (4, 3) || s == MazeState (4, 2)

      // Maze is discrete
      def discretize (s: MazeState): MazeFiniteState =  s

      private def mazeReward (s: MazeState): MazeReward = s match
        case MazeState (4, 3) => 10.0   // Good final state
        case MazeState (4, 2) => -10.0  // Bad final state
        case MazeState (_, _) => -1.0


      def distort (a: MazeAction): Randomized[MazeAction] = a match
         case Up | Down => Randomized.oneOf (Left, Right)
         case Left | Right => Randomized.oneOf (Up, Down)


      def successor (s: MazeState) (a: MazeAction): MazeState =
         require (valid (s))
         val result = a match
            case Up => s.copy (y = s.y+1)
            case Down => s.copy (y = s.y-1)
            case Left => s.copy (x = s.x-1)
            case Right => s.copy (x = s.x+1)
         if valid (result) then result else s

      def valid (s: MazeState): Boolean =
         s.x >= 1 && s.x <= 4 && s.y >= 1 && s.y <= 3 && s != MazeState (2, 2)

      val attention = 0.8

      def step (s: MazeState) (a: MazeAction): Randomized[(MazeState, MazeReward)] =
         for
            precise <- Randomized.coin (attention)
            action <- if precise then Randomized.const (a) else distort (a)
            newState = successor (s) (action)
         yield (newState, mazeReward (newState))

      def initialize: Randomized[MazeState] =
         Randomized.oneOf (instances.allFiniteStates.filter { !isFinal (_) }:_*)

      override def zeroReward: MazeReward = 0

      val instances = MazeInstances

end Maze


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MazeInstances
   extends AgentConstraints[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized]:

   given enumAction: BoundedEnumerable[MazeAction] =
      BoundedEnumerableFromList (Up, Down, Left, Right)

   given enumState: BoundedEnumerable[MazeFiniteState] =
      val ss = for
         y <- List (1, 2, 3)
         x <- List (1, 2, 3, 4)
         result = MazeState (x, y)
         if Maze.valid (result)
      yield result
      BoundedEnumerableFromList (ss: _*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genMazeState: Gen[MazeState] = for
      y <- Gen.choose (1, 3)
      x <- Gen.choose (1, 4)
      if (x != 2 && y != 2)
   yield MazeState (x = x.abs, y = y.abs)

   given arbitraryState: Arbitrary[MazeState] = Arbitrary (genMazeState)

   given eqMazeState: Eq[MazeState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[MazeReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[MazeReward] = Arith.given_Arith_Double

end MazeInstances
