package symsim
package concrete

import cats.kernel.BoundedEnumerable

trait ConcreteExactRL[State, ObservableState, Action]
  extends ExactRL[State, ObservableState, Action, Double, Randomized2]:

  import agent.instances.*
  import agent.instances.given

  /** A value function implementation */
  override val vf: ConcreteQTable[ObservableState, Action] = 
    new ConcreteQTable[ObservableState, Action]
  import vf.*

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait

  def episodes: Int

  // TODO: unclear if this is general (if it turns out to be the same im
  // symbolic or approximate algos we should promote this to the trait
  
  given rng: probula.RNG

  def runQ: (Q, List[Q]) =
    val initials = agent.initialize.sample (episodes)
    val outcome = learn (vf.initialize, List[VF] (), initials).sample ()
    (outcome._1, outcome._2)

  override def run: Policy =
    qToPolicy (this.runQ._1)

  /** Convert the matrix Q after training into a Policy map. */
  def qToPolicy (q: Q) (using Ordering[Double]): Policy =
    q.states.map { s => (s, vf.bestAction (q) (s)) }.to (Map)

  /** Evaluate the policy `p` on `episodes` episodes generated using the
   *  agent's initialization function.
   *
   *  @return A distribution of distributions of rewards. The structure is
   *  deliberately kept nested so that you can distinguish between marginals
   *  for each initial state. The outer distribution is over initial states,
   *  the inner distribution is over randomness in the learning
   *  process/environment.
   */
  def evaluate (p: Policy): Randomized2[Randomized2[Double]] =
    evaluate (p, agent.initialize)
