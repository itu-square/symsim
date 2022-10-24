package symsim
package concrete

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

type UnitState = Unit
type UnitAction = Unit
type UnitReward = Double

object UnitAgent
   extends Agent[UnitState, UnitState, UnitAction, UnitReward, Randomized]:
      override def isFinal (s: UnitState): Boolean = true
      override def observe (s: UnitState): UnitState =  s
      override def step (s: UnitState) (a: UnitAction): Randomized[(UnitState, UnitReward)] =
        Randomized.const (() -> 0.1)
      override def initialize: Randomized[UnitState] = Randomized.const (())
      override def zeroReward: UnitReward = 0.0
      override val instances = UnitInstances


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object UnitInstances
   extends AgentConstraints[UnitState, UnitState, UnitAction, UnitReward, Randomized]:
   given enumAction: BoundedEnumerable[UnitAction] = BoundedEnumerableFromList (())
   given enumState: BoundedEnumerable[UnitState] = BoundedEnumerableFromList (())
   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad
   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable
   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized
   lazy val genUnitState: Gen[UnitState] = Gen.const (())
   given arbitraryState: Arbitrary[UnitState] = Arbitrary (genUnitState)
   given eqUnitState: Eq[UnitState] = Eq.fromUniversalEquals
   given arbitraryReward: Arbitrary[UnitReward] = Arbitrary (Gen.double)
   given rewardArith: Arith[UnitReward] = Arith.arithDouble
