package symsim.concrete

import scala.annotation.targetName

import org.scalacheck.{Gen, Prop}

import scala.jdk.StreamConverters.*
import scala.util.Try

import java.security.SecureRandom

/** A purely functional wrapping of java.security.SecureRandom (not really pure,
 *  but hides things sufficiently for now).
  */
type Randomized[+A] = LazyList[A]

type Probability = Double


/** A purely functional wrapping of java.security.SecureRandom.  Presents
 *  generators as possibly finite streams (a source that dries out), which can
 *  be extended to continue producing infinitely with repeat (infinite).  There
 *  is some loss of referential transparency in this switch to infinity
 *  (repeat).
  *
  * Randomized should be a non-branching scheduler.  For the infrastructure to
  * work, all our schedulers need to be finitely branching.
  **/
object Randomized:

  /** Create a generator that produces a single 'a'. Used to create
    * deterministic values when a scheduler/randomized type is
    * expected.
    */
  def const[A] (a: =>A): Randomized[A] =
    LazyList (a)

  def prob: Randomized[Probability] =
    LazyList (SecureRandom ().nextDouble)


  /** Produce a single random number between the bounds, 
    * right exclusive 
    **/
  def between (minInclusive: Int, maxExclusive: Int): Randomized[Int] =
    LazyList (SecureRandom ()
      .ints (minInclusive, maxExclusive)
      .findAny
      .getAsInt)


  def between (minInclusive: Double, maxExclusive: Double): Randomized[Double] =
    LazyList (SecureRandom ()
      .doubles (minInclusive, maxExclusive)
      .findAny
      .getAsDouble)

  def gaussian (mean: Double = 0.0, stddev: Double = 1.0): Randomized[Double] =
    LazyList (SecureRandom ().nextGaussian * stddev + mean)

  /** Toss a coing biased towards true with probabilty 'bias' */
  def coin (bias: Probability): Randomized[Boolean] =
    LazyList (SecureRandom ().nextDouble <= bias)

  def oneOf[A] (choices: A*): Randomized[A] =
    between (0, choices.size)
      .map { i => choices (i) }

  def eachOf[A] (choices: A*): Randomized[A] = 
    LazyList(choices*)

  def repeat[A] (ra: =>Randomized[A]): Randomized[A] =
    LazyList.continually (ra).flatten

  given randomizedIsMonad: cats.Monad[Randomized] =
    cats.instances.lazyList.catsStdInstancesForLazyList

  given canTestInRandomized: symsim.CanTestIn[Randomized] =
    new symsim.CanTestIn[Randomized] {

      @targetName ("toPropBoolean")
      def toProp (rProp: Randomized[Boolean]) =
        Prop.forAllNoShrink (toGen (rProp)) (identity[Boolean])

      // This is a nasty hack that costs as a lot on memory in tests (but
      // probably not in experiments).  Unfortunately, I do not see an easy way
      // to add a completely new generator for scalacheck that encapsulates
      // Randomized.  The Gen class appears to be sealed and pimping cannot add
      // state to objects?
      def toGen[A] (ra: => Randomized[A]): Gen[A] =
        require (ra.nonEmpty)
        val stream = repeat (ra)
        Gen.choose(0, 1000)
          .map { i => stream (i) }
    }

  /** This extensions should ideally be used at a  top-level of the program, 
   *  as they loose the type annotation for the side effect of randomness.
   */
  extension [A] (self: Randomized[A])
    /** Get one sample from randomized. Note that the sample will be random but
     *  always the same if you call several times. 
     *  (at least in the current implementation)
     */
    def sample (): A = sample(1).head

    /** Get n samples from randomized. Note that the sample will be random but
     *  always the same if you call several times. 
     *  (at least in the current implementation)
     *
     *  Note: as long as we have not refactored the underlying implementation
     *  of Randomized, this is unsafe. It is possible that randomized does 
     *  not have n samples.
     *
     */
    def sample (n: Int): LazyList[A] = self.take(n)

    /** Perform an imperative operation that depends on one sample from this
     *  Randomized.  This is mostly meant for IO at this point.
     */
    def run (f: A => Unit): Unit = f(self.sample ())


  extension (self: Randomized[Double])

    /** Calculate a mean of this random variable. 
     *  Note that calling mean might be very expensive, if obtaining each sample
     *  is expensive. 
     */
    def mean (support: Int = 100): Double = 
      val finite = self.take (support) 
      finite.sum / support.toDouble

    def variance (support: Int = 100): Double = 
      val finite = self.take (support)
      val μ = finite.mean (support)
      self.map (x => (x - μ)*(x -μ))
        .mean (support)

    /** Calculate a mean and variance of a sample of size support. This is done
     *  in one go, so it is more efficient than calling mean and variance
     *  separately. 
     */
    def meanVar (support: Int = 100): (Double, Double) = 
      val finite = self.take (support)
      val μ = finite.mean (support)
      val σσ = self.map (x => (x - μ)*(x -μ))
                   .mean (support)
      (μ, σσ)

