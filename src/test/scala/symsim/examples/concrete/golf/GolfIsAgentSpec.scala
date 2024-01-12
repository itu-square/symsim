package symsim
package examples.concrete.golf

import laws.AgentLaws
import laws.EpisodicLaws

private val golf = new Golf (using spire.random.rng.SecureJava.apply)

class GolfIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.golf.Golf is an Agent", AgentLaws (golf).laws)
  checkAll ("concrete.golf.Golf is Episodic", EpisodicLaws (golf).laws)
