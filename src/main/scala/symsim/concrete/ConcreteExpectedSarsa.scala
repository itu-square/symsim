package symsim
package concrete

case class ConcreteExpectedSarsa[State, FiniteState, Action] (
     val agent: Agent[State, FiniteState, Action, Double, Randomized],
     val alpha: Double,
     val gamma: Double,
     val epsilon: Probability,
     val episodes: Int,
) extends ExpectedSarsa[State, FiniteState, Action, Double, Randomized]
    with ConcreteExactRL[State, FiniteState, Action]:

    override def toString: String =
        s"ExpectedSarsa-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon, $episodes episodes)"