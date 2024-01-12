package symsim
package concrete

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

type UnitState = Unit
type UnitAction = Unit
type UnitReward = Double

class UnitAgent (using probula.RNG)
   extends Agent[UnitState, UnitState, UnitAction, UnitReward, Randomized2]:
      override def isFinal (s: UnitState): Boolean = true
      override def observe (s: UnitState): UnitState =  s
      override def step (s: UnitState) (a: UnitAction): Randomized2[(UnitState, UnitReward)] =
        Randomized2.const (() -> 0.1)
      override def initialize: Randomized2[UnitState] = Randomized2.const (())
      override val instances = new UnitInstances


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
class UnitInstances (using probula.RNG)
  extends AgentConstraints[UnitState, UnitState, UnitAction, UnitReward, Randomized2]:
  given enumAction: BoundedEnumerable[UnitAction] = BoundedEnumerableFromList (())
  given enumState: BoundedEnumerable[UnitState] = enumAction
  given schedulerIsMonad: Monad[Randomized2] = Randomized2.randomizedIsMonad
  given canTestInScheduler: CanTestIn[Randomized2] = Randomized2.canTestInRandomized
  lazy val genUnitState: Gen[UnitState] = Gen.const (())
  given arbitraryState: Arbitrary[UnitState] = Arbitrary (genUnitState)
  given eqUnitState: Eq[UnitState] = Eq.fromUniversalEquals
  given arbitraryReward: Arbitrary[UnitReward] = Arbitrary (Gen.double)
  given rewardArith: Arith[UnitReward] = Arith.arithDouble
