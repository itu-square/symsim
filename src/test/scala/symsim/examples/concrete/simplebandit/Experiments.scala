package symsim
package examples.concrete.simplebandit

class Experiments
   extends ExperimentSpec[BanditState,BanditState,BanditAction]:

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = BanditObj,
     alpha = 0.1,
     gamma = 1.0,
     epsilon = 0.05,
     episodes = 150,
   )

   s"SimpleBandit experiment with ${sarsa}" in {

      val policy = learnAndLog (sarsa)
   }
