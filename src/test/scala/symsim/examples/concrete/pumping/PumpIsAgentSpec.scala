package symsim
package examples.concrete.pumping

import symsim.laws.AgentLaws
import symsim.laws.EpisodicLaws

class PumpIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.pumping.Pump is an Agent", AgentLaws (Pump).laws)
