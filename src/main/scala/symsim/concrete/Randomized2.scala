package symsim.concrete

import scala.annotation.targetName
import org.scalacheck.{Gen, Prop}
import probula.{Dist, RNG}

opaque type Randomized2[+A] = Dist[A]

object Randomized2:

  /** Create a generator that produces a single 'a'. Used to create
    * deterministic values when a scheduler/randomized type is
    * expected.
    */
  def const[A] (a: =>A): Randomized2[A] =
    probula.Dirac (a)

  def prob: Randomized2[Double] = 
   probula.UniformC(0.0, 1.0) 

  def between (minInclusive: Int, maxExclusive: Int): Randomized2[Int] =
    probula.Uniform(minInclusive, maxExclusive+1)

  def between (minInclusive: Double, maxExclusive: Double): Randomized2[Double] =
    probula.UniformC(minInclusive, maxExclusive)

  def gaussian (mean: Double = 0.0, stdDev: Double = 1.0): Randomized2[Double] =
    probula.Gaussian(mean, stdDev)

  def coin (bias: Probability): Randomized2[Boolean] =
    probula.Bernoulli(bias)

  def oneOf[A] (choices: A*): Randomized2[A] =
    probula.Uniform(choices*)

  /** For this representation of Randomized, repeat does nothing (an identity),
   *  we should probably remove repeat from the API. */
  def repeat[A] (ra: =>Randomized2[A]): Randomized2[A] = ra


  given randomizedIsMonad: cats.Monad[Randomized2] = new cats.Monad[Randomized2]:
    def flatMap[A, B](fa: Randomized2[A])(f: A => Randomized2[B]): Randomized2[B] =
      fa.flatMap(f)
    def pure[A](x: A): Randomized2[A] = 
      probula.Dirac[A] (x)

    import probula.{Name, IData, RNG}

    def tailRecM[A, B](ini: A)(f: A => Randomized2[Either[A, B]]): Randomized2[B] = 
      val a0 = f(ini)
      new Dist[B]:
        def name: Name = Name.Suffixed(a0.name, "tailRecM")
        def sample[C >: B](using RNG): IData[C] =
          val newChain = summon[cats.Monad[LazyList]]
            .tailRecM[A, B](ini) { a => f(a).sample.chain }
          IData(name, newChain)

  given canTestInRandomized (using RNG): symsim.CanTestIn[Randomized2] =
    new symsim.CanTestIn[Randomized2] {

      @targetName ("toPropBoolean")
      def toProp (rProp: Randomized2[Boolean]) =
        Prop.forAllNoShrink (toGen (rProp)) (identity[Boolean])

      // This is a nasty hack that costs as a lot on memory in tests (but
      // probably not in experiments).  Unfortunately, I do not see an easy way
      // to add a completely new generator for scalacheck that encapsulates
      // Randomized.  The Gen class appears to be sealed and pimping cannot add
      // state to objects?
      def toGen[A] (ra: => Randomized2[A]): Gen[A] =
        val list: LazyList[A] = ra.sample.chain
        Gen.choose(0, 1000)
          .map { i => list (i) }
    }


  /** This extensions should ideally be used at a  top-level of the program, 
   *  as they loose the type annotation for the side effect of randomness.
   */
  extension [A] (self: Randomized2[A])
    /** Get one sample from randomized. Note that the sample will be random but
     *  always the same if you call several times. 
     *  (at least in the current implementation)
     */
    def sample () (using RNG): A = 
      self.sample(1)(using summon[RNG]).chain.head

    /** Get n samples from randomized. Note that the sample will be random but
     *  always the same if you call several times. 
     *  (at least in the current implementation)
     *
     *  Note: as long as we have not refactored the underlying implementation
     *  of Randomized, this is unsafe. It is possible that randomized does 
     *  not have n samples.
     *
     */
    def sample (n: Int) (using RNG): LazyList[A] = 
      self.sample(n).chain.take(n)

    /** Perform an imperative operation that depends on one sample from this
     *  Randomized.  This is mostly meant for IO at this point.
     */
    def run (f: A => Unit): Unit = f(self.sample ())

    def filter (p: A => Boolean): Randomized2[A] = 
      self.filter (p)
