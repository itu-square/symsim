package symsim
package examples.concrete.breaking

class BreakingExperiments
  extends org.scalatest.freespec.AnyFreeSpec:

  "Breaking Car experiment" in {

    // Import evidence that states and actions can be enumerated
    import Car._

    val sarsa = symsim.concrete.ConcreteSarsa (
      agent = Car,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ratio
      episodes = 100000,
    )

    val q = sarsa.runQ
    val policy = sarsa.qToPolicy (q)
    val policy_output = sarsa.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = sarsa.pp_Q (q).hang (4)
    info (q_output.render (80))

  }
