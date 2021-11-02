package symsim
package examples.concrete.simplemaze

class Experiments
   extends org.scalatest.freespec.AnyFreeSpec:

   "SimpleMaze test run" in {

      val sarsa = symsim.concrete.ConcreteSarsa (
        agent = Maze,
        alpha = 0.1,
        gamma = 1.0,
        epsilon = 0.05,
        episodes = 10000,
      )

      val q = sarsa.runQ
      val policy = sarsa.qToPolicy (q)
      val policy_output = sarsa.pp_policy (policy).hang (4)
      info (policy_output.render (80))
      val q_output = sarsa.pp_Q (q).hang (4)
      info (q_output.render (80))
   }

end Experiments
