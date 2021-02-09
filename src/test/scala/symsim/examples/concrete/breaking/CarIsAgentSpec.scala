package symsim
package examples.breaking.concrete

import cats.kernel.BoundedEnumerable

// TODO likely removable, clean up imports in this file
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import symsim.examples.concrete.breaking._
import symsim.concrete.Randomized

import symsim.laws.discipline.AgentTests

import symsim.examples.concrete.breaking.instances._

class CarIsAgentSpec extends SymSimSpec {

  checkAll ("concrete.breaking.Car is an Agent",
    AgentTests[CarState, CarFiniteState, CarAction, CarReward, Randomized].agent (Car))

}

