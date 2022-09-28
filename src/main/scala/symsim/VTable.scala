package symsim

import org.scalacheck.Gen

trait VTable[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ValueFunction[State, ObservableState, Action, Reward, Scheduler]:

  this: ExactRL[State, ObservableState, Action, Reward, Scheduler] =>

  import agent.instances.*

  type V = Map[ObservableState, Reward]
  type VF = V

  /** Construct a zero initialized value table */
  def initialize: V =
    allObservableStates
      .map { (_, agent.zeroReward) }
      .toMap

  /** Generate total value tables for testing. */
  val genV: Gen[V] =
    val N = agent.instances.allObservableStates.size
    Gen.listOfN[Reward] (N, agent.instances.arbitraryReward.arbitrary)
      .map { agent.instances.allObservableStates.zip }
      .map { _.toMap }
