package symsim
package examples.concrete.breaking

import symsim.concrete.ConcreteSarsa

class Experiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

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
      distraction = 0.05, // explore vs exploit ration.
      epochs = 5000,
      seed = 1000
    )

    val policy = sarsa.run

  }

}
