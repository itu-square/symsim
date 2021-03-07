package symsim
package laws.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait SarsaTests[State, FiniteState, Action, Reward, Scheduler[_]]
  extends org.typelevel.discipline.Laws {

  implicit def evReward: Arith[Reward]

  def laws (s: Sarsa[State, FiniteState, Action, Reward, Scheduler]) =
    new symsim.laws.SarsaLaws (s)

  def sarsa (s: Sarsa[State, FiniteState, Action, Reward, Scheduler])
    (implicit arbUnit: Arbitrary[Unit], eqUnit: Eq[Unit]): RuleSet =
    new SimpleRuleSet (
      "sarsa",
      // TODO: deactivated tentatively
      // "initQ produces a zero matrix of the right size" ->
      //   laws (s).initQRightSize,
      // TODO: remove this one soon
      "sanity always passes law" ->
        forAll (laws (s).alwaysPassesSanity _)
    )
}

object SarsaTests {

  def apply[State, FiniteState, Action, Reward: Arith, Scheduler[_]]
  : SarsaTests[State, FiniteState, Action, Reward, Scheduler] =
    new SarsaTests[State, FiniteState, Action, Reward, Scheduler] {
      override def evReward = implicitly[Arith[Reward]]
    }
}
