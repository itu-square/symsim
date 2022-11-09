package symsim
package examples.concrete.braking

import laws.AgentLaws
import laws.EpisodicLaws

class CarIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.braking.Car is an Agent", AgentLaws (Car).laws)
  checkAll ("concrete.braking.Car is Episodic", EpisodicLaws (Car).laws)
