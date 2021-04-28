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
    if policy.isEmpty then Doc.empty
    else
      val w1 = policy.keys.map { _.toString.length }.max
      val w2 = policy.values.map { _.toString.length }.max
      val horizontal = Doc.tabulate ('.', "-+-",
        List ("+" + "-" * (1 + w1) -> Doc.str ("-" * (1 + w2) + "+")))
      val rows = policy
        .toList
        .map { (s,r) => ("| " + s.toString.padTo (w1, ' '),  Doc.str (r.toString.padTo (w2, ' ')) + Doc.str (" |")) }
        .sortBy { _._1 }
      horizontal / Doc.tabulate ('.', " | ", rows) / horizontal
