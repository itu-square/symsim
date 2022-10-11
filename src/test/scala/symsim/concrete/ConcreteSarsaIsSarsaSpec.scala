package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar
import symsim.laws.SarsaLaws
import symsim.laws.ConcreteSarsaLaws

class ConcreteSarsaIsSarsaSpec
  extends SymSimSpec:

  val csarsa = ConcreteSarsa (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 1.0,
    epsilon = 0.05,
    episodes = 500,
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa", SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteSarsa is ConcreteSarsa", ConcreteSarsaLaws (csarsa).laws)
