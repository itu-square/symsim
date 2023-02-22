package symsim

import org.scalacheck.Gen

import symsim.concrete.Probability 

trait ValueFunction[ObservableState, Action, Reward, Scheduler[_]]:
  type VF
  
  /** The current reward estimation for state action pair (s, a) */
  def value (vf: VF) (s: ObservableState, a: Action): Reward

  /** The current probability of taking action a in state s, on policy 
   *
   *  For off-policy algorithms this probability is 1 for the best action (see
   *  below @bestAction) and 0 for all other actions.
   */
  def probability (ε: Probability) (vf: VF) (s: ObservableState, a: Action): Probability

  /** argmax action selection */
  def bestAction (vf: VF) (s: ObservableState): Action

  /** On policy action selection */
  def chooseAction (ε: Probability) (vf: VF) (s: ObservableState): Scheduler[Action]

  def initialize: VF

  def genVF: Gen[VF]
