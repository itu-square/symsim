package symsim
package laws.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class SarsaTests[State, FiniteState, Action, Reward, Scheduler[_]]
  (s: Sarsa[State, FiniteState, Action, Reward, Scheduler])
    extends org.typelevel.discipline.Laws
{

  type S = Sarsa[State, FiniteState, Action, Reward, Scheduler]

  val laws = new symsim.laws.SarsaLaws (s)

  def sarsa: RuleSet = new SimpleRuleSet (
      "sarsa",
      // TODO: deactivated tentatively
      // "initQ produces a zero matrix of the right size" ->
      //   laws.initQRightSize,
      // TODO: remove this one soon
      "sanity always passes law" ->
        forAll (laws.alwaysPassesSanity _)
    )
}
