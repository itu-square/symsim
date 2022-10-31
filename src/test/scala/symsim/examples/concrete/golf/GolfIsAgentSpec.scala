package symsim
package examples.concrete.golf

import laws.AgentLaws
import laws.EpisodicLaws

class GolfIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.golf.Golf is an Agent", AgentLaws (Golf).laws)
  checkAll ("concrete.golf.Golf is Episodic", EpisodicLaws (Golf).laws)
