package symsim
package examples.concrete.cliffwalking

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

case class CWState (x: Int, y: Int):
    override def toString: String = s"($x,$y)"

type CWObservableState = CWState
type CWReward = Double

enum CWAction:
    case Up, Down, Left, Right

    override def toString: String = this match
        case CWAction.Down => "↓"
        case CWAction.Up => "↑"
        case CWAction.Left => "←"
        case CWAction.Right => "→"

object CliffWalking
  extends Agent[CWState, CWObservableState, CWAction, CWReward, Randomized]
    with Episodic:

    /** The episode should be guaranteed to terminate after
      * TimeHorizon steps. This is used *only* *for* testing. It does
      * not actually termintae the episodes. It is a bug if they run
      * longer.
      */
    val TimeHorizon: Int = 2000

    def isFinal (s: CWState): Boolean =
        s.y == 0 && s.x > 0

    def observe (s: CWState): CWObservableState =
        CWState (s.x, s.y)

    def move (s: CWState, a: CWAction): CWState =
        require(s.x >= 0)
        require(s.x <= 11)
        require(s.y >= 0)
        require(s.y <= 3)

        a match
            case CWAction.Up => CWState (s.x, (s.y + 1).min(3))
            case CWAction.Down => CWState (s.x, (s.y - 1).max(0))
            case CWAction.Right => CWState ((s.x + 1).min(11), s.y)
            case CWAction.Left => CWState ((s.x - 1).max(0), s.y)


    def CWReward (s: CWState) (a: CWAction): CWReward =
        if s.y == 0 && s.x > 0 && s.x < 11
        then -100.0
        else -1.0


    def step (s: CWState) (a: CWAction): Randomized[(CWState, CWReward)] =
        val s1 = move (s, a)
        Randomized.const (s1, CWReward (s1) (a))

    def initialize: Randomized[CWState] = for
        x <- Randomized.repeat (Randomized.between (0, 11))
        y <- Randomized.between (0, 3)
//        y <- Randomized.repeat (Randomized.between (0, 3))
        s = CWState (x, y) if !isFinal (s)
    yield s

    override def zeroReward: CWReward = 0

    val instances = CliffWalkingInstances

end CliffWalking

/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object CliffWalkingInstances
  extends AgentConstraints[CWState, CWObservableState, CWAction, CWReward, Randomized]:

    given enumAction: BoundedEnumerable[CWAction] =
        BoundedEnumerableFromList (CWAction.Up, CWAction.Down, CWAction.Left, CWAction.Right)

    given enumState: BoundedEnumerable[CWObservableState] =
        val ss = for
            x <- Seq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
            y <- Seq (0, 1, 2, 3)
        yield CWState (x, y)
        BoundedEnumerableFromList (ss*)


    given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

    val schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

    given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

    lazy val genCWState: Gen[CWState] = for
        x <- Gen.oneOf (Seq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        y <- Gen.oneOf (Seq (0, 1, 2, 3))
//        x <- Gen.choose(0, 11)
//        y <- Gen.choose(0, 3)
    yield CWState (x, y)

    given arbitraryState: Arbitrary[CWState] = Arbitrary (genCWState)

    given eqCWState: Eq[CWState] = Eq.fromUniversalEquals

    given arbitraryReward: Arbitrary[CWReward] = Arbitrary (Gen.double)

    given rewardArith: Arith[CWReward] = Arith.arithDouble

end CliffWalkingInstances
