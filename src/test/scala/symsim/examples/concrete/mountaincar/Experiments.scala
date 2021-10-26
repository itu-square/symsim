package symsim
package examples.concrete.mountaincar

import symsim.concrete.ConcreteSarsa

class MountainCarExperiments
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks:

  "test run" in {

    // Import evidence that states and actions can be enumerated
    import MountainCar._

    val sarsa = ConcreteSarsa[
      CarState,
      CarFiniteState,
      CarAction
    ] (
      agent = MountainCar,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ration.
      episodes = 100000,
    )

    val policy = sarsa.run

  }
