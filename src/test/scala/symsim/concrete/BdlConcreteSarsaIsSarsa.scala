package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

private val mountainCar = 
  new MountainCar (using spire.random.rng.SecureJava.apply)

import mountainCar.instances.enumState
import mountainCar.instances.enumAction

/** This test is just a sanity check - it mostly tests Bdl against
 *  itself (so an equality check).
 */
class BdlConcreteSarsaIsSarsaSpec
  extends SymSimSpec:

  val csarsa = BdlConcreteSarsa (
    agent = mountainCar,
    alpha = 0.1,
    gamma = 0.2,
    epsilon0 = 0.0, // the update distribution test requires low ε for stability
    episodes = 1000,
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteSarsa is ConcreteSarsa",
    symsim.laws.ConcreteSarsaLaws (csarsa, csarsa.gamma).laws)
