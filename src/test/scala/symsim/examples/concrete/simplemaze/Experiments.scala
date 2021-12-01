package symsim
package examples.concrete.simplemaze

class Experiments
   extends org.scalatest.freespec.AnyFreeSpec
   with org.scalatest.matchers.should.Matchers:

   "SimpleMaze experiment" in {

      val sarsa = symsim.concrete.ConcreteSarsa (
        agent = Maze,
        alpha = 0.1,
        gamma = 1.0,
        epsilon = 0.05,
        episodes = 3000,
      )

      val q = sarsa.runQ
      val policy = sarsa.qToPolicy (q)
      val policy_output = sarsa.pp_policy (policy).hang (4)
      info (policy_output.render (80))
      val q_output = sarsa.pp_Q (q).hang (4)
      info (q_output.render (80))

      val groundTruth = List (
         (1,1) -> Up,
         (1,2) -> Up,
         (1,3) -> Right,
         (2,1) -> Left,
         (2,3) -> Right,
         (3,1) -> Left,
         (3,2) -> Up,
         (3,3) -> Right,
         (4,1) -> Left,
         (4,2) -> Right,
         (4,3) -> Right,
      ).toMap

      policy should be (groundTruth)
   }
