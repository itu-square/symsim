package symsim
package laws

import scala.language.postfixOps

import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary

import breeze.stats.distributions.{Rand, Beta, Gaussian}
import breeze.stats.distributions.Rand.VariableSeed.*

import symsim.concrete.ConcreteExactRL
import symsim.concrete.ConcreteQTable
import symsim.concrete.Randomized

/**
 * Laws that have to be obeyed by any refinement of symsim.ConcreetSarsa
 *
 * The gamma argument needs to be provided separately, because
 * ConcreteExactRL might be an algorithm that does not have a single
 * gamma (this interface is quite abstract). We presently do not have
 * an interface for algorithms with a single discount factor, so we
 * have to use ConcreteExactRL or Sarsa/ConcreteSarsa. The latter is
 * too concrete, as it would disallow running the testsuite on the
 * BDL-based implementations, which is a convenient sanity check (the
 * test of bdl against bdl should pass).
 *
 * TODO: Why is this just for Concrete? Does it have to?
 */
case class ConcreteSarsaLaws[State, ObservableState, Action] 
  (sarsa: ConcreteExactRL[State, ObservableState, Action], 
   gamma: Double
  ) extends org.typelevel.discipline.Laws:

  import sarsa.{agent,vf}
  import sarsa.agent.instances.given
  import sarsa.vf.{Q, apply, updated}

  val γ = gamma

  // A shortcut for instantiating the interpreter with the right term for SARSA
  val bdl =  
    symsim.concrete.BdlConcreteSarsa[State, ObservableState, Action] 
      (agent, sarsa.α, this.γ, sarsa.ε, sarsa.episodes)

  given Arbitrary[Q] = 
    Arbitrary (vf.genVF (using agent.instances.arbitraryReward))
  
  val laws: RuleSet = SimpleRuleSet (
    "concreteSarsa",

    "probability of not choosing the best action is smaller than ε" ->
      forAllNoShrink { (q: Q, a_t: Action) =>
        
        val trials = for 
          s_t  <- agent.initialize
          a_tt <- vf.chooseAction (sarsa.ε) (q) (agent.observe (s_t))
        yield a_tt != vf.bestAction (q) (agent.observe (s_t))

        // We implement this as a bayesian test, checking whether htere is
        // 0.95 belief that the probability of suboptimal action is ≤ ε.
        // We check this by computing the posterior and asking CDF (ε) ≥ 0.95
        // Using the number of episodes as the #trials is a bit of an abuse

        val successes = trials.take (sarsa.episodes).count { _ == true }
        val failures = sarsa.episodes - successes

        // α=1 and β=1 gives a prior, weak flat, unbiased
        val cdfEpsilon =  Beta (2 + successes, 2 + failures).cdf (sarsa.ε)

        (cdfEpsilon >= 0.995) :| 
          s"""|The beta posterior test results (failing):
              |    posterior_cdf(${sarsa.ε}) == $cdfEpsilon
              |    #exploration selections ≠ best action: $successes 
              |    #best action selections: $failures 
              |    #total trials: $sarsa.episodes""".stripMargin
    },

    "The update distribution produced by an update follows Eq. 14 (BDL)" ->
       forAllNoShrink { (q_t: Q, s_t: State, a_t: Action) =>
         
         // #samples for the distribution test
         val n = 80000 

         val os_t = agent.observe (s_t)

         // call the tested implementation
         val sut: Randomized[(Q, State, Action)] = 
           Randomized.repeat (sarsa.learningEpoch (q_t, s_t, a_t))

         // call the spec interpreter
         val spec: Randomized[(Q, State, Action)] = 
           Randomized.repeat (bdl.learningEpoch (q_t, s_t, a_t))

         // val spec = Randomized.repeat(for 
         //   (s_tt,r_tt) <- agent.step (s_t) (a_t)
         //   os_tt        = agent.observe (s_tt)
         //   a_tt        <- sarsa.vf.chooseAction (sarsa.ε) (q_t) (os_tt)
         //   g_tt         = r_tt + gamma * q_t (os_tt, a_tt)
         //   u            = q_t (os_t, a_t) - sarsa.α * (g_tt - q_t (os_t, a_t))
         //   q_tt         = q_t.updated (os_t, a_t, u)
         // yield (q_tt, s_tt, a_tt))
         
         // Feb 18 variant (that passes on Feb 18 code!)
         // val spec = Randomized.repeat (for 
         //   (s_tt,r_tt) <- agent.step (s_t) (a_t)
         //   os_tt        = agent.observe (s_tt)
         //   a_tt         <- sarsa.vf.chooseAction (sarsa.ε) (q_t) (os_tt)
         //   g_tt         = r_tt + gamma * q_t (os_tt, a_tt)
         //   u            = q_t (os_t, a_t) - sarsa.alpha * (g_tt - q_t (os_t, a_t))
         //   q_tt         = q_t.updated (os_t, a_t, u)
         // yield (q_tt, s_tt, a_tt))

         // We do this test by assuming that both distributions are normal 
         // (A generalized test with StudentT would be even better).
         // We find a Bayesian posterior (analytically) for the difference of
         // their means, and checking whether we believe the difference
         // is close to zero with small standard deviation.
         //
         // I followed this note for the conjugate prior update:
         // https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
         // also John Kruschke, Doing Bayesian Data Analysis, 2nd Ed.
         // p. 453 (the formulation below is from there).
         // See also Christian P. Robert. The Bayesian Choice. Springer. p. 121. 
         //
         // Extract a univariate distributions over updates
         
        
         val sutUpdates = sut.map { (q_tt, _, _) => q_tt (os_t, a_t) }.take (n)
         val specUpdates = spec.map { (q_tt, _, _) => q_tt (os_t, a_t) }.take (n)

         // TODO // TODO debugging section
         // TODO // assert (sutUpdates.size == n, s"ala ma kota ${sutUpdates.size}")
         // TODO println ("q_t (os_t, a_t): " + q_t(os_t, a_t))
         // TODO println ("SUT:" + sutUpdates.take(15).map {_.toString}.mkString (", "))
         // TODO println ("BDL:" + specUpdates.take(15).map {_.toString}.mkString (", "))

         val sutMean = sutUpdates.sum / n.toDouble
         val specMean = specUpdates.sum / n.toDouble

         // Prior, we assume zero reward, with large standard deviation
         // It would be nice to know something about the range of possible rewards
         // We do have some rwards in the range of 10e+4.
         val μ_0 = 0.0
         val σ2_0 = 10.0e+4 

         // Likelihood (μ distributed with the prior, σ fixed for now)
         // Isn't it fair to fix a small σ2 because the estimation of
         // mean should be very stable? The literature is a bit quiet
         // on how to select σ2 for this estimator of posterior μ.
         val σ2 = 1.0

         // Posterior params for sample of size `n` with mean `mean`
         def μ_post (n: Int, mean: Double) = 
           val nd = n.toDouble
           (1.0/σ2_0 / (nd/σ2 + 1.0/σ2_0)) * μ_0 + 
             (nd/σ2 / (nd/σ2 + 1.0/σ2_0)) * mean 

         // τ is 1/σ
         def τ_post (n: Int) =
           val nd = n.toDouble
           1.0/σ2_0+ nd/σ2

         val μ_post_sut = μ_post (n, sutMean)
         val σ2_post_sut = 1.0 / τ_post (n)

         val μ_post_spec = μ_post (n, specMean)
         val σ2_post_spec = 1.0 / τ_post (n)

         // Sum (difference) of two Normally distributed variables 
         // to estimate the difference of means (Gaussian again).
         // https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables
         val μ_diff = μ_post_sut - μ_post_spec
         val σ_diff = σ2_post_sut + σ2_post_spec

         // Is the expectation of the difference of the means close to zero?
         val tolerance = 0.05
         val cdfDiff = Gaussian (μ_diff, σ_diff).cdf (tolerance) 

         val msg = s"""|The normal posterior test results (failing):
                       |    posterior_cdf(${tolerance}) == $cdfDiff
                       |    "μ_diff: $μ_diff, σ_diff: $σ_diff""".stripMargin

         (cdfDiff >= 0.995) :| msg
       }

  )
