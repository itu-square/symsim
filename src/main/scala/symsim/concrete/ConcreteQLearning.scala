package symsim
package concrete

case class ConcreteQLearning[State, ObservableState, Action] (
   val agent: Agent[State, ObservableState, Action, Double, Randomized],
   val alpha: Double,
   val gamma: Double,
   val epsilon: Probability,
   val episodes: Int,
) extends QLearning[State, ObservableState, Action, Double, Randomized]
   with ConcreteExactRL[State, ObservableState, Action]:

      override def toString: String =
         s"Q-Learn(α=$alpha, 𝛾=$gamma, 𝜀=$epsilon, $episodes episodes)"
