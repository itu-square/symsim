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
 * Each action achieves the intended effect with probability 0.8;
 * the rest of the time the action moves the agent at right angles
 * of the intended direction
 *
 * If the agent bumps into a wall, it stays in the same square
 *
 * Reward: -0.04 for all states except the terminal states
 * (which have rewards +1 and -1). The reward for a path is
 * the sum of the rewards for its ststaes
 *
 * Optimal policy (Russell, Norvig, Fig 17.2, p. 648):
*
 *     Right, Right, Right, Terminal,
 *     Up,           Up,    Terminal,
 *     Up,    Left,  Left,  Left
 *
 */

type MazeState = (Int, Int, Int)

type MazeObservableState = (Int, Int)
type MazeReward = Double

sealed trait MazeAction
case object Left extends MazeAction
case object Right extends MazeAction
case object Up extends MazeAction
case object Down extends MazeAction


object Maze
  extends 
    Agent[MazeState, MazeObservableState, MazeAction, MazeReward, Randomized],
    Episodic:

  val TimeHorizon: Int = 2000
  val TimeLimit: Int = 10

  def isFinal (s: MazeState): Boolean =
    (s._1, s._2) == (4, 3) || (s._1, s._2) == (4, 2) || s._3 == 100

  // Maze is discrete
  def observe (s: MazeState): MazeObservableState = (s._1, s._2)

  // We are not using the original reward function from AIAMA as it
  // gives to unstable learning results
  private def mazeReward (s: MazeState): MazeReward = (s._1, s._2) match
    case (4, 3) => +0.0     // Good final state
    case (4, 2) => -1000.0   // Bad final state (dead)
    case (_, _) => -1.0


  def distort (a: MazeAction): Randomized[MazeAction] = a match
    case Up | Down => Randomized.oneOf (Left, Right)
    case Left | Right => Randomized.oneOf (Up, Down)


  def successor (s: MazeState) (a: MazeAction): MazeState =
    require (valid (s))
    val result = a match
      case Up    => (s._1, s._2+1, s._3+1)
      case Down  => (s._1, s._2-1, s._3+1)
      case Left  => (s._1-1, s._2, s._3+1)
      case Right => (s._1+1, s._2, s._3+1)
    if valid (result) then result else s

  def valid (s: MazeState): Boolean =
     s._1 >= 1 && s._1 <= 4 && s._2 >= 1 && s._2 <= 3 && (s._1, s._2) != (2, 2)

  val attention = 0.8

  def step (s: MazeState) (a: MazeAction): Randomized[(MazeState, MazeReward)] =
    for
      precise <- Randomized.coin (attention)
      action <- if precise then Randomized.const (a) else distort (a)
      newState = successor (s) (action)
    yield (newState, mazeReward (newState))

  def initialize: Randomized[MazeState] = for
    x <- Randomized.repeat(Randomized.between(1, 4))
    y <- Randomized.between(1, 3)
    t = 0
    s = (x, y, t) if !isFinal(s) && valid (s)
  yield s

  val instances = MazeInstances

end Maze


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MazeInstances
   extends AgentConstraints[MazeState, MazeObservableState, MazeAction, MazeReward, Randomized]:

   given enumAction: BoundedEnumerable[MazeAction] =
      BoundedEnumerableFromList (Up, Down, Left, Right)

   given enumState: BoundedEnumerable[MazeObservableState] =
      val ss = for
         y <- List (1, 2, 3)
         x <- List (1, 2, 3, 4)
         result = (x, y, 0)
         if Maze.valid (result)
      yield (result._1, result._2)
      BoundedEnumerableFromList (ss*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genMazeState: Gen[MazeState] = for
      y <- Gen.choose (1, 3)
      x <- Gen.choose (1, 4)
      t = 0
      if (x != 2 && y != 2)
   yield (x.abs, y.abs, t)

   given arbitraryState: Arbitrary[MazeState] = Arbitrary (genMazeState)

   given eqMazeState: Eq[MazeState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[MazeReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[MazeReward] = Arith.arithDouble

end MazeInstances
