package symsim
package examples.concrete.cliffWalking

import laws.AgentLaws
import laws.EpisodicLaws

private val cliffWalking = 
  new CliffWalking (using spire.random.rng.SecureJava.apply)

class CliffWalkingIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.cliffWalking.CliffWalking is an Agent", AgentLaws (cliffWalking).laws)
  checkAll ("concrete.cliffWalking.CliffWalking is Episodic", EpisodicLaws (cliffWalking).laws)
