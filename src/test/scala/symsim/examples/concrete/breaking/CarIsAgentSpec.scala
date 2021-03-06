package symsim
package examples.breaking.concrete

import symsim.examples.concrete.breaking._

import symsim.concrete.Randomized
import symsim.examples.concrete.breaking.instances._


class CarIsAgentSpec extends SymSimSpec {

  import symsim.concrete.Randomized.propInScheduler

  checkAll ("concrete.breaking.Car is an Agent",
    symsim.laws.discipline.AgentTests[
      CarState,
      CarFiniteState,
      CarAction,
      CarReward,
      Randomized
    ].agent (Car))

}
