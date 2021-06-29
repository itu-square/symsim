package symsim
package examples.concrete.windygrid

import symsim.concrete.ConcreteSarsa
import examples.concrete.windygrid.GridAction._

class Experiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "test run" in {

    // Import evidence that states and actions can be enumerated
    import WindyGrid._

    val sarsa = ConcreteSarsa[
      GridState,
      GridFiniteState,
      GridAction
    ] (
      agent = WindyGrid,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.1, // explore vs exploit ration.
      epochs = 5000
    )

    val q = sarsa.runQ
    val policy = sarsa.qToPolicy (q)
    val policy_output = sarsa.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = sarsa.pp_Q (q).hang (4)
    info (q_output.render (80))

  }

}
