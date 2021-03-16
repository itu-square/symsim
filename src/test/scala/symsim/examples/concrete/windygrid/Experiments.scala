package symsim
package examples.concrete.windygrid

import symsim.concrete.ConcreteSarsa
import examples.concrete.windygrid.GridAction._

class Experiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "test run" ignore {

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
      distraction = 0.05, // explore vs exploit ration.
      epochs = 5000,
      seed = 1000
    )

    val policy = sarsa.run

  }

}
