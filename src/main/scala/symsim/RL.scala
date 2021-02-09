package symsim

import cats.kernel.BoundedEnumerable

trait RL[FiniteState, Action] {

  type Policy = Map[FiniteState,Action]

  /** This is the actual reinforcement learning algortihm call. */

  def run: Policy
}
