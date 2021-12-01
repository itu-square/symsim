package symsim
package concrete

class UnitAgentExperiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers:

  "test run (should not crash)" in {

    // Import evidence that states and actions can be enumerated
    import UnitAgent._

    val sarsa = symsim.concrete.ConcreteSarsa (
      agent = UnitAgent,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ratio
      episodes = 500,
    )

    val q = sarsa.runQ
    val policy = sarsa.qToPolicy (q)
    val policy_output = sarsa.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = sarsa.pp_Q (q).hang (4)
    info (q_output.render (80))

    policy should be (List (() -> ()).toMap)
  }
