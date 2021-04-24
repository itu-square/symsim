package symsim
package examples.concrete.simplemaze

import symsim.concrete.ConcreteSarsa
import org.typelevel.paiges.Doc

class Experiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks:

  "test run" in {

    // Import evidence that states and actions can be enumerated
    import Maze._

    val sarsa = ConcreteSarsa[
      MazeState,
      MazeFiniteState,
      MazeAction
    ] (
      agent = Maze,
      alpha = 0.1,
      gamma = 1.0,
      distraction = 0.05, // explore vs exploit ratio
      epochs = 100000,
      seed = 1000
    )

    val q = sarsa.runQ
    val policy = sarsa.qToPolicy (q)
    val policy_output = sarsa.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = sarsa.pp_Q (q).hang (4)
    info (q_output.render (80))

  }
