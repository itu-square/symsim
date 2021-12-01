package symsim

trait ExperimentSpec[State, FiniteState, Action]
   extends org.scalatest.freespec.AnyFreeSpec
   with org.scalatest.matchers.should.Matchers:

   def learnAndLog (setup: concrete.ConcreteExactRL[State, FiniteState, Action]) =
      val q = setup.runQ
      val policy = setup.qToPolicy (q)
      val policy_output = setup.pp_policy (policy).hang (4)
      info (policy_output.render (80))
      val q_output = setup.pp_Q (q).hang (4)
      info (q_output.render (80))
      policy
