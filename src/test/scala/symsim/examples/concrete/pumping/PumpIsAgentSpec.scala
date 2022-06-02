package symsim
package examples.concrete.pumping

import laws.AgentLaws
import laws.EpisodicLaws

class PumpIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.pumping.Pump is an Agent", AgentLaws (Pump).laws)
   checkAll ("concrete.pumping.Pump is Episodic", EpisodicLaws (Pump).laws)
