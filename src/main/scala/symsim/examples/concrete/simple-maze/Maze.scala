package symsim
package examples.concrete.simple-maze

import symsim.concrete.Randomized


object Maze
  extends Agent[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized] {

    def isFinal (s: MazeState): Boolean =
      s.x == 4 && (s.y == 2 || s.y == 3)

    // Maze is already discrete 
    def discretize (s: MazeState): MazeFiniteState =  { s }

    private def mazeReward (s: MazeState) (a: MazeAction): MazeReward =
      if (s == MazeState(x=4,y=3)) 10 //Good final state
      else if (s == MazeState(x=4,y=2)) -10 // Bad final state
      else -1

    // Does this even make sense for the maze? 
    /** Granularity of the step in seconds */
    // private val t: Double = 1.0

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: MazeState) (a: MazeAction): (MazeState, MazeReward) = { // Do cases over MazeAction
      a match
        case Up =>    (stepUp(s),   1)
        case Down =>  (stepDown,    1)
        case Left =>  (stepLeft(s), 1)
        case Right => (stepRight(s),1)
}

// I would like to select the step-function based on an enumeration type of actions
    def stepUp(s: MazeState): MazeState =
      val y1 = if (s.x == 2 || s.y = 3) then s.y else s.y+1
      mazeState(x=s.x,y=y1)

    def stepDown(s: MazeState): MazeState =
      val y1 = if (s.x == 2 || s.y = 1) then s.y else s.y-1
      mazeState(x=s.x,y=y1)

    def stepLeft(s: MazeState): MazeState =
      val x1 = if (s.y == 2 || s.x = 1) then s.x else s.x-1
      mazeState(x=x1,y=s.y)

    def stepRight(s: MazeState): MazeState =
      val x1 = if (s = MazeState(y=1,x=2) || s.x = 4) then s.x else s.x-1
      mazeState(x=x1,y=s.y)

    def initialize: Randomized[MazeState] = for {
      y <- Randomized.between (1, 3)
      x <- Randomized.between (1, 4)
      s0 = MazeState (x,y)
      s <- if (isFinal (s0) || s == MazeState(2,2)) initialize
           else Randomized.const (s0)
    } yield s

    override def zeroReward: MazeReward = 0

    lazy val instances = MazeInstances
}


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MazeInstances
  extends AgentConstraints[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized] {

  import cats.{Eq, Monad}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  implicit lazy val enumAction: BoundedEnumerable[MazeAction] =
    BoundedEnumerableFromList (-10, -5, -2.5)

  implicit lazy val enumState: BoundedEnumerable[MazeFiniteState] = {
    val ss = for {
        y <- Seq (1, 2, 3)
        x <- Seq (1, 2, 3, 4)
    } yield MazeState (x,y)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genMazeState: Gen[MazeState] = for {
      y <- arbitrary[Int]
      x <- arbitrary[Int]
  } yield MazeState (y=Math.abs (y), x=Math.abs (x))

  implicit lazy val arbitraryState: Arbitrary[MazeState] =
    Arbitrary (genMazeState)

  implicit lazy val eqMazeState: Eq[MazeState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.double)

  implicit lazy val rewardArith: Arith[MazeReward] =
    Arith.arithDouble
}
