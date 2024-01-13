package symsim
package concrete

import cats.kernel.BoundedEnumerable

case class ConcreteQLearningWithDecay [
  State,
  ObservableState: BoundedEnumerable,
  Action: BoundedEnumerable
] (

  val agent: Agent[State, ObservableState, Action, Double, Randomized2],
  val alpha: Double,
  val gamma: Double,
  val epsilon0: Probability,
  val episodes: Int,

) (using val rng: probula.RNG) 
  extends QLearning[State, ObservableState, Action, Double, Randomized2],
  ConcreteExactRL[State, ObservableState, Action],
  BoundedEpsilonDecay:

  override def toString: String =
    s"Q-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon0, $episodes episodes)"
