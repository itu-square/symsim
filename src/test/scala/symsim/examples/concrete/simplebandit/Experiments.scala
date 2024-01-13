package symsim
package examples.concrete.simplebandit

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply

class Experiments
  extends ExperimentSpec[BanditState,BanditState,BanditAction]:

  val sarsaConst = symsim.concrete.ConcreteSarsa (
    agent = BanditObjConst,
    alpha = 0.1,
    gamma = 0.0,
    epsilon0 = 0.05,
    episodes = 8000,
  )

  s"SimpleBandit experiment with ${sarsaConst}" in {
    val policies = learnAndLog (sarsaConst)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val results = eval (sarsaConst, policies)
    results.save ("simplebandit-const.csv")
  }
  
  val sarsaGaussian = symsim.concrete.ConcreteSarsa (
    agent = BanditObjGaussian,
    alpha = 0.01,
    gamma = 0.0,
    epsilon0 = 0.1,
    episodes = 1000,
  )

  s"SimpleBandit experiment with ${sarsaGaussian}" in {
    val policies = learnAndLog (sarsaGaussian)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val results = eval (sarsaGaussian, policies)
    results.save ("simplebandit-gaussian.csv")
  }
