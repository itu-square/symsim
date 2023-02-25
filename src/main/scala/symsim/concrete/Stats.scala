package symsim.concrete

import breeze.stats.distributions.{Rand, Beta, Gaussian}
import breeze.stats.distributions.Rand.VariableSeed.*

         
/** Update a gausian prior with a (pre-conjugate) Gaussian likelihood.
 *
 * I followed this note for the conjugate prior update:
 * https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
 * also John Kruschke, Doing Bayesian Data Analysis, 2nd Ed.
 * p. 453 (the formulation below is from there).
 * See also Christian P. Robert. The Bayesian Choice. Springer. p. 121. 
 *
 * TODO: check if this is not provided by breeze already; if so
 * remove
 *
 * @return the posterior mean and variance
 */
def GaussianPosterior (prior_μ: Double, prior_σ2: Double, likelihood_σ2: Double)
  (observations: Seq[Double]): (Double, Double) = 

  val n = observations.size
  val observationMean = observations.sum / n.toDouble

  // Posterior params for sample of size `n` with mean `mean`
  def μ_post (n: Int, mean: Double) = 
    val nd = n.toDouble
    (1.0/prior_σ2 / (nd/likelihood_σ2 + 1.0/prior_σ2)) * prior_μ + 
      (nd/likelihood_σ2 / (nd/likelihood_σ2 + 1.0/prior_σ2)) * mean 

  // τ is 1/σ
  def τ_post (n: Int) =
    val nd = n.toDouble
    1.0/prior_σ2 + nd/likelihood_σ2

  val post_μ = μ_post (n, observationMean)
  val post_σ2 = 1.0 / τ_post (n)

  (post_μ, post_σ2)



/** Probability mass between two points in a Gaussian distribution 
 *
 *  TODO: check if this is not provided by breeze already; if so
 *  remove
 */
def GaussianMass (μ: Double, σ2: Double) (left: Double, right: Double)
  : Probability =
  val mass_right  = Gaussian (μ, σ2).cdf (right)
  val mass_left = Gaussian (μ, σ2).cdf (left) 
  mass_right - mass_left

