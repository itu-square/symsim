package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

import MountainCar.instances.given

class ConcreteSarsaIsSarsaSpec
  extends SymSimSpec:

  val csarsa = ConcreteSarsa (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 0.2,
    epsilon = 0.05,
    episodes = 1000,
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteSarsa is ConcreteSarsa",
    symsim.laws.ConcreteSarsaLaws (csarsa, csarsa.gamma).laws)
