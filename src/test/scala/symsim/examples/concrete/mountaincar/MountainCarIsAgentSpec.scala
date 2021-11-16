package symsim
package examples.concrete.mountaincar

import laws.AgentLaws

class MountainCarIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.mountaincar.MountainCar is an Agent", AgentLaws (MountainCar).laws)
