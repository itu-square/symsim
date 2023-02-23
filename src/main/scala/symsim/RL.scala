package symsim

import cats.kernel.BoundedEnumerable
import cats.syntax.option.*
import org.typelevel.paiges.Doc

trait RL[ObservableState, Action, Reward, Scheduler[_]]:

  def vf: ValueFunction[ObservableState, Action, Reward, Scheduler]

  type Policy = Map[ObservableState, Action]

  /** The actual reinforcement learning algortihm call. */
  def run: Policy

  /**
    * Pretty print the policy, for instance to inspect it after training.
    *
    * @param p the policy to pretty print
    * @return the typelevel paiges document representing the result (you can
    * Doc.render it to create a string)
    *
    * TODO: should go away from here, if Policy becomes a type on its own
    */
  def pp_policy (policy: Policy): Doc =
    if policy.isEmpty then Doc.empty
    else
      val rows = List("state", "action") ::policy
       .toList
       .map[List[String]] { (k,v) => k.toString ::v.toString ::Nil }
       .sortBy (_.head)
      symsim.tabulate (' ', " | ", rows, "-".some, "-+-".some)
