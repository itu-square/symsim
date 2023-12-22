package symsim
package concrete

import cats.kernel.BoundedEnumerable

trait ConcreteExactRL[State, ObservableState, Action]
  extends ExactRL[State, ObservableState, Action, Double, Randomized]:

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

  def runQ: (Q, List[Q]) =
    val initials = Randomized.repeat (agent.initialize).take (episodes)
    val schedule = learn (vf.initialize, List[VF](), initials)
    (schedule.head._1, schedule.head._2)

  override def run: Policy =
    qToPolicy (this.runQ._1)

  /** Convert the matrix Q after training into a Policy map. */
  def qToPolicy (q: Q) (using Ordering[Double]): Policy =
    q.states.map { s => (s, vf.bestAction (q) (s)) }.to (Map)

  def policyEval (policies: List[Policy]): List[List[Double]] =
    val initials = Randomized.repeat (agent.initialize).take (5)
    val rL = for
      s_t <- initials
    yield policies.map (policy => runEval (policy, s_t, 0.0))
    rL.toList

  def runEval(policy: Policy, s_t: State, r_acc: Double): Double =
    if agent.isFinal(s_t) then r_acc
    else
      val os_t = agent.observe (s_t)
      val a = if policy.get (os_t) != None then policy (os_t)
              else allActions.head
      val s_r = for
        sa_tt <- agent.step(s_t)(a)
        r_tacc = sa_tt._2 + r_acc
      yield (sa_tt._1, r_tacc)
      val s_tt = s_r.head._1
      val r_acc1 = s_r.head._2
      runEval(policy, s_tt, r_acc1)
