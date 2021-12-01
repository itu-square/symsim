package symsim
package concrete

case class ConcreteQLearning[State, FiniteState, Action] (
   val agent: Agent[State, FiniteState, Action, Double, Randomized],
   val alpha: Double,
   val gamma: Double,
   val epsilon: Probability,
   val episodes: Int,
) extends QLearning[State, FiniteState, Action, Double, Randomized]
   with ConcreteExactRL[State, FiniteState, Action]:

      override def toString: String =
         s"Q-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon, $episodes episodes)"
