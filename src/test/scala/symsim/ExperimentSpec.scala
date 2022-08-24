package symsim

trait ExperimentSpec[State, ObservableState, Action]
   extends org.scalatest.freespec.AnyFreeSpec
   with org.scalatest.matchers.should.Matchers:

   def learnAndLog (setup: concrete.ConcreteExactRL[State, ObservableState, Action]) =
      val q = setup.runQ
      val policy = setup.qToPolicy (q)
      val policy_output = setup.pp_policy (policy).hang (4)
      info (policy_output.render (80))
      val q_output = setup.pp_Q (q).hang (4)
      info (q_output.render (80))
      policy
