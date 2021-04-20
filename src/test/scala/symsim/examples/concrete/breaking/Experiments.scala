package symsim
package examples.concrete.breaking

import symsim.concrete.ConcreteSarsa
import org.typelevel.paiges.Doc

class Experiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks:

  "test run" ignore {

    // Import evidence that states and actions can be enumerated
    import Car._

    val sarsa = ConcreteSarsa[
      CarState,
      CarFiniteState,
      CarAction
    ] (
      agent = Car,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ratio
      epochs = 5000,
      seed = 1000
    )

    val q = sarsa.runQ
    val policy = sarsa.qToPolicy (q)
    val policy_output = sarsa.pp_policy (policy).hang (4)
    info (policy_output.render (80))
    val q_output = sarsa.pp_Q (q).hang (4)
    info (q_output.render (80))

  }
