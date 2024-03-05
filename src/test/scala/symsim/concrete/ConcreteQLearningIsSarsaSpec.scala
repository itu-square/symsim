package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

private val mountainCar = 
  new MountainCar (using spire.random.rng.SecureJava.apply)
import mountainCar.instances.{enumAction, enumState}

/** The name of this test might be amusing, but since the test 
 *  only exercise single epoch properties, a QLearning 
 *  implementation should behave exactly like SARSA for them.
 */
  
class ConcreteQLearningIsSarsaSpec
  extends SymSimSpec:

  val qLearning = ConcreteQLearning (
    agent = mountainCar,
    alpha = 0.1,
    gamma = 0.2,
    epsilon0 = 0.0, // The update distribution test requires low ε for stability
    episodes = -1, // Not used in this test
  )

  checkAll ("concrete.ConcreteQLearning is Sarsa", 
    symsim.laws.SarsaLaws (qLearning).laws)
  checkAll ("concrete.ConcreteQLearning is ConcreteSarsa",
    symsim.laws.ConcreteSarsaLaws (qLearning, qLearning.gamma).laws)
