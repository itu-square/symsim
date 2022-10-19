package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

class ConcreteSarsaIsSarsaSpec
  extends SymSimSpec:

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  val csarsa = ConcreteSarsa (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 1.0,
    epsilon = 0.5,
    episodes = 1000,
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteSarsa is ConcreteSarsa",
    symsim.laws.ConcreteSarsaLaws (csarsa).laws)
