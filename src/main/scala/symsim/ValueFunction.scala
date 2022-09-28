package symsim

import org.scalacheck.Gen
trait ValueFunction[State, ObservableState, Action, Reward, Scheduler[_]]:
  type VF
  def bestAction (vf: VF) (s: State): Action
  def chooseAction (vf: VF) (s: State): Scheduler[Action]
  def initialize: VF
  def genVF: Gen[VF]
