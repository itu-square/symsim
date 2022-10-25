package symsim

import org.scalacheck.Gen
trait ValueFunction[ObservableState, Action, Reward, Scheduler[_]]:
  type VF
  def bestAction (vf: VF) (s: ObservableState): Action
  def chooseAction (vf: VF) (s: ObservableState): Scheduler[Action]
  def initialize: VF
  def genVF: Gen[VF]
