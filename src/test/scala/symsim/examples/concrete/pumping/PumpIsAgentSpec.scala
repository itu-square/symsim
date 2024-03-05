package symsim
package examples.concrete.pumping

import laws.AgentLaws
import laws.EpisodicLaws

private val pump = new Pump (using spire.random.rng.SecureJava.apply)

class PumpIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.pumping.Pump is an Agent", AgentLaws (pump).laws)
  checkAll ("concrete.pumping.Pump is Episodic", EpisodicLaws (pump).laws)
