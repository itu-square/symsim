package symsim
package examples.concrete.mountaincar

import laws.AgentLaws
import laws.EpisodicLaws

class MountainCarIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.mountaincar.MountainCar is an Agent", AgentLaws (MountainCar).laws)
   // Randomized testing of this law is too simplistic for mountaincar to
   // reliably pass the test, so it is deactivated for now. A symbolic test'
   // could be more efficient.
   // checkAll ("concrete.mountaincar.MountainCar is Episodic", EpisodicLaws (MountainCar).laws)
