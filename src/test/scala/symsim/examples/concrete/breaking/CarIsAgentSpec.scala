package symsim
package examples.concrete.breaking

import laws.discipline.{AgentTests, EpisodicTests}

class CarIsAgentSpec extends SymSimSpec:
   checkAll ("concrete.breaking.Car is an Agent", new AgentTests ().agent (Car))
   checkAll ("concrete.breaking.Car is Episodic", new EpisodicTests ().agent (Car))
