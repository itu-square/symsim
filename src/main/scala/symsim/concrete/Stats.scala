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


/** Compute an HDI of mass p for density Beta(α, β).
 *
 *  This is up to some precision (because there is a numerical step in there,
 *  bisection). 
 *
 *  Source: Andrey Akinshin. Trimmed Harrell-Davis quantile
 *  estimator based on the highest density interval of the given width.  
 *
 *  whuber (https://stats.stackexchange.com/users/919/whuber), Credible set for
 *  beta distribution, URL (version: 2015-07-05):
 *  https://stats.stackexchange.com/q/160035
 *
 *  (but we do given probability mass not given width).
 *
 *  We only handle nondegenerate cases, with α and β bigger than one.
 *  TODO: check if this is not provided by breeze already
 *
 *  @param α the number of successes observed (+1)
 *  @param β the number of failures observed (+1)
 *  @param p the probability mass of the sought interval
 *  @param precision the precision of floating point equality for numerical
 *                   search
 *  @return an HDI of size `size` for density β(α, β).
 */
def betaHdi (α: Double, β: Double, p: Double, precision: Double = 1e-9): Double = 
  require (α > 1.0 && β > 1.0)
  val distr = Beta (α, β)
  val mode = distr.mode
  val l = distr.inverseCDF (p / 2.0)
  val r = distr.inverseCDF (p / 2.0 + 1 - α)
  // We need to find a zero in 2D space of this function
  def f (l: Double, r: Double): Double = 
    val diff = distr.pdf (l) - distr.pdf (r)
    diff * diff
  ???
