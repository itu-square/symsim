package symsim

import symsim.concrete.ConcreteQTable

trait ExperimentSpec[State, Observable, Action]
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatest.matchers.should.Matchers:

  def learnAndLog (setup: concrete.ConcreteExactRL[State, Observable, Action]
    & ConcreteQTable[State, Observable, Action]) =
    val q = setup.runQ
    val policy = setup.qToPolicy (q)
    val policy_output = setup.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = setup.pp_Q (q).hang (4)
    info (q_output.render (80))
    policy
