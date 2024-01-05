package symsim
package concrete

import cats.kernel.BoundedEnumerable

case class ConcreteQLearning [
  State,
  ObservableState: BoundedEnumerable,
  Action: BoundedEnumerable
] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized],
  val alpha: Double,
  val gamma: Double,
  val epsilon0: Probability,
  val episodes: Int,
) extends QLearning[State, ObservableState, Action, Double, Randomized],
  ConcreteExactRL[State, ObservableState, Action],
  NoDecay:

  override def toString: String =
    s"Q-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon0, $episodes episodes)"
