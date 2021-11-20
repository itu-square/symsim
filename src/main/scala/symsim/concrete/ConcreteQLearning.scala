package symsim
package concrete

case class ConcreteQLearning[State, FiniteState, Action] (
   val agent: Agent[State, FiniteState, Action, Double, Randomized],
   val alpha: Double,
   val gamma: Double,
   val epsilon: Probability,
   val episodes: Int,
) extends QLearning[State, FiniteState, Action, Double, Randomized]
   with ConcreteExactRL[State, FiniteState, Action]
