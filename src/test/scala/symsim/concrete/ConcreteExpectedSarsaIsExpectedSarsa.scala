package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

class ConcreteExpectedSarsaIsSarsaSpec
  extends SymSimSpec:

  val csarsa = ConcreteExpectedSarsa (
    agent = new MountainCar,
    alpha = 0.1,
    gamma = 0.2,
    epsilon0 = 0.003, // The update distribution test requires low ε for stability
    episodes = -1, // Not used in this test
  )

  checkAll ("concrete.ConcreteExpectedSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteExpectedSarsa is ConcreteExpectedSarsa",
    symsim.laws.ConcreteExpectedSarsaLaws (csarsa, csarsa.gamma).laws)
