package symsim
package examples.concrete.cliffWalking

import laws.AgentLaws
import laws.EpisodicLaws

class CliffWalkingIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.cliffWalking.CliffWalking is an Agent", AgentLaws (CliffWalking).laws)
  checkAll ("concrete.cliffWalking.CliffWalking is Episodic", EpisodicLaws (CliffWalking).laws)
