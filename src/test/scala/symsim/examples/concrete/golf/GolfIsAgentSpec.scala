package symsim
package examples.concrete.golf

import laws.AgentLaws
import laws.EpisodicLaws

class GolfIsAgentSpec
  extends SymSimSpec:
  
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  checkAll ("concrete.golf.Golf is an Agent", AgentLaws (Golf).laws)
  checkAll ("concrete.golf.Golf is Episodic", EpisodicLaws (Golf).laws)
