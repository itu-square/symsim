package symsim
package examples.concrete.cliffwalking

import laws.AgentLaws

class CliffWalkingIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.cliffwalking.CliffWalking is an Agent", AgentLaws (CliffWalking).laws)
