package symsim
package concrete

case class ConcreteExpectedSarsa[State, ObservableState, Action] (
 val agent: Agent[State, ObservableState, Action, Double, Randomized],
 val alpha: Double,
 val gamma: Double,
 val epsilon0: Probability,
 val episodes: Int,
) extends ExpectedSarsa[State, ObservableState, Action, Double, Randomized],
  ConcreteExactRL[State, ObservableState, Action],
  NoDecay:

  override def toString: String =
    s"ExpectedSarsa-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon0, $episodes episodes)"
