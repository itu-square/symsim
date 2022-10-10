package symsim

import symsim.concrete.ConcreteQTable

trait ExperimentSpec[State, ObservableState, Action]
  extends org.scalatest.freespec.AnyFreeSpec,
  org.scalatest.matchers.should.Matchers:

   def learnAndLog (setup: concrete.ConcreteExactRL[State, ObservableState, Action]
     with ConcreteQTable[State, ObservableState, Action],
     outputQTable: Boolean = true,
     outputPolicy: Boolean = true) =

     val q = setup.runQ
     val policy = setup.qToPolicy (q)
     val policy_output = setup.pp_policy (policy).hang (4)
     if outputQTable then info (policy_output.render (80))
     val q_output = setup.pp_Q (q).hang (4)
     if outputPolicy then info (q_output.render (80))
     policy
