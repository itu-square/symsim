package symsim
package examples.concrete.braking

import laws.AgentLaws
import laws.EpisodicLaws

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val car = new Car 

class CarIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.braking.Car is an Agent", AgentLaws (car).laws)
  checkAll ("concrete.braking.Car is Episodic", EpisodicLaws (car).laws)
