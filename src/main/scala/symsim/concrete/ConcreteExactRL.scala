package symsim
package concrete

import cats.kernel.BoundedEnumerable

trait ConcreteExactRL[State, ObservableState, Action]
   extends ExactRL[State, ObservableState, Action, Double, Randomized]:

  import agent.instances.*

  def epsilon: Probability

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait

  def episodes: Int

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait

  def bestAction (q: Q) (s: State): Action =
    val qs = q (agent.discretize (s)) map { _.swap }
    qs (qs.keys.max)


  /** Action selection policy based on argMax and randomization.  */
  def chooseAction (q: Q) (s: State): Randomized[Action] =
    for
      explore <- Randomized.coin (this.epsilon)
      action <- if explore
        then Randomized.oneOf (allActions: _*)
        else Randomized.const (bestAction (q) (s))
    yield action



  /** Construct a zero initialized Q matrix */
  def initQ: Q =
    // Create the initial Q matrix (zero's everywhere)
    val qa = allActions
      .map { a => (a, agent.zeroReward) }
      .toMap

    val q0 = allObservableStates
      .map { state => (state, qa) }
      .toMap

    q0


  def runQ: Q =
     val initials = Randomized.repeat (agent.initialize).take (episodes)
     val schedule = learn (this.initQ, initials)
     schedule.head

  override def run: Policy =
     qToPolicy (this.runQ)
