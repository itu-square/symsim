package symsim

import org.scalacheck.Gen

trait VTable[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ValueFunction[ObservableState, Action, Reward, Scheduler]:

  this: ExactRL[State, ObservableState, Action, Reward, Scheduler] =>

  type V = Map[ObservableState, Reward]
  type VF = V

  /** Construct a zero initialized value table */
  def initialize: V =
    this.agent.instances.allObservableStates
      .map { (_, this.agent.instances.rewardArith.zero) }
      .toMap

  /** Generate total value tables for testing. */
  val genV: Gen[V] =
    val N = this.agent.instances.allObservableStates.size
    Gen.listOfN[Reward] (N, agent.instances.arbitraryReward.arbitrary)
      .map { this.agent.instances.allObservableStates.zip }
      .map { _.toMap }
