package symsim

import cats.kernel.BoundedEnumerable
import org.typelevel.paiges.Doc

trait RL[FiniteState, Action]:

  type Policy = Map[FiniteState,Action]

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
    val rows = policy
      .toIterable
      .map { case (k,v) => (k.toString, Doc.str (v)) }
    Doc.tabulate ('.', "...", rows)
