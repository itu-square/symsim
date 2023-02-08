package symsim

import org.scalacheck.Gen

trait ValueFunction[ObservableState, Action, Reward, Scheduler[_]]:
  type VF
  def value (vf: VF) (s: ObservableState, a: Action): Reward
  def bestAction (vf: VF) (s: ObservableState): Action
  def bestActionValue (vf: VF) (s: ObservableState): Reward = 
    value (vf) (s, bestAction (vf) (s))
  def chooseAction (vf: VF) (s: ObservableState): Scheduler[Action]
  def initialize: VF
  def genVF: Gen[VF]
