package symsim
package examples.concrete.simplebandit

class Experiments
   extends ExperimentSpec[BanditState,BanditState,BanditAction]:

   val sarsaConst = symsim.concrete.ConcreteSarsa (
     agent = BanditObjConst,
     alpha = 0.1,
     gamma = 0.0,
     epsilon = 0.05,
     episodes = 8000,
   )

   s"SimpleBandit experiment with ${sarsaConst}" in {
     val policies = learnAndLog(sarsaConst)
     evalAndLog(sarsaConst, policies, "simplebandit.csv")
   }
   
   val sarsaGaussian = symsim.concrete.ConcreteSarsa (
     agent = BanditObjGaussian,
     alpha = 0.01,
     gamma = 0.0,
     epsilon = 0.1,
     episodes = 1000,
   )

   s"SimpleBandit experiment with ${sarsaGaussian}" in {
     val policies = learnAndLog(sarsaGaussian)
     evalAndLog(sarsaGaussian, policies, "simplebandit.csv")
   }
