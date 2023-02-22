package symsim
package concrete

import cats.kernel.BoundedEnumerable

trait ConcreteExactRL[State, ObservableState, Action]
  extends ExactRL[State, ObservableState, Action, Double, Randomized]:

  this: ConcreteQTable [ObservableState, Action] =>

  import agent.instances.*

  def epsilon: Probability

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait

  def episodes: Int

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait

  def runQ: VF =
    val initials = Randomized.repeat (agent.initialize).take (episodes)
    val schedule = learn (this.initialize, initials)
    schedule.head

  override def run: Policy =
    qToPolicy (this.runQ)

  /** Convert the matrix Q after training into a Policy map. */
  def qToPolicy (q: Q) (using Ordering[Double]): Policy =
    q.states.map { s => (s, bestAction (q) (s)) }.to (Map)
