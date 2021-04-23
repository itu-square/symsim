package symsim
package concrete

import cats.kernel.BoundedEnumerable

case class ConcreteSarsa [
  State,
  FiniteState,
  Action
] (

  val agent: Agent[State, FiniteState, Action, Double, Randomized],
  val alpha: Double,
  val gamma: Double,
  val distraction: Probability,
  val epochs: Int,
  val seed: Long

) extends Sarsa[State, FiniteState, Action, Double, Randomized] {


  import agent.instances._


  // TODO: it is a bit unclear if this is general (if it turns out to be the
  // same im symbolic SARSA we should promote this to the trait

  def bestAction (q: Q) (s: State): Action = {
    val qs = q (agent.discretize (s)) map { _.swap }
    val M = qs.keys.max
    qs (M)
  }



  /** Action selection policy based on argMax and randomzation.  */
  def chooseAction (q: Q) (s: State): Randomized[Action] =
    for
      distracted <- Randomized.coin (distraction)
      action <- if distracted
                then Randomized.oneOf (agent.instances.enumAction.membersAscending)
                else Randomized.const (bestAction (q) (s))
    yield action



  /** Construct a zero initialized Q matrix */
  def initQ: Q = {
    // Create the initial Q matrix (zero's everywhere)
    val qa = BoundedEnumerable[Action]
      .membersAscending
      .map { a => (a, agent.zeroReward) }
      .toMap

    val q0 = BoundedEnumerable[FiniteState]
      .membersAscending
      .map { state => (state, qa) }
      .toMap

    q0
  }



  def runQ: Q =
    learnN (epochs, initQ)
      // Got a Randomized[QS], run it from the seed
      .runA (new scala.util.Random (seed))
      // Get the value out of run (which returns an Eval)
      .value

  override def run: Policy = qToPolicy (this.runQ)

}
