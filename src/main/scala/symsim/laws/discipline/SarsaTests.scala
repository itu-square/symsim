package symsim
package laws.discipline

import cats.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class SarsaTests[State, FiniteState, Action, Reward, Scheduler[_]]
  (s: Sarsa[State, FiniteState, Action, Reward, Scheduler])
    extends org.typelevel.discipline.Laws:

  val laws = new symsim.laws.SarsaLaws (s)

  def sarsa: RuleSet = new SimpleRuleSet (
      "sarsa",

      "initQ defined for all FiniteStates" ->
        laws.initQDefinedForAllFiniteStates,

      "initQ defined for all Actions for each source state" ->
        laws.initQDefinedForAllActions,

      "initQ contains only zeroRewards" ->
        laws.initQAllValuesZero,

      "generated Q matrices are total for finite state space" ->
        laws.generatedQMatricesAreTotalWRTStateSpace,

      "generated Q matrices are total for action state space" ->
        laws.generatedQMatricesAreTotalWRTActionSpace,

      "chooseAction (q) (s) ∈ Action for all q and s" ->
        laws.chooseActionGivesEnumerableAction,
    )
