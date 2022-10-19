package symsim
package examples.concrete.breaking

import laws.AgentLaws
import laws.EpisodicLaws

class CarIsAgentSpec
  extends SymSimSpec:

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  checkAll ("concrete.breaking.Car is an Agent", AgentLaws (Car).laws)
  checkAll ("concrete.breaking.Car is Episodic", EpisodicLaws (Car).laws)
