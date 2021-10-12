package symsim.concrete

import cats.data.State
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

import scala.jdk.StreamConverters._
import scala.util.Try

import java.security.SecureRandom

/** A purely functional wrapping of scala.util.Random */
type Randomized[A] = LazyList[A]

type Probability = Double

/** A purely functional wrapping of scala.util.Random. Delegations.
  * The implementations use LazyList unfold rather than conversions from
  * Java streams as this seems much more memory efficient.
  **/
object Randomized:

   /** Create a generator that always a. Used to create deterministic
     * values when a scheduler/randomized type is expected.
     */
   def const[A] (a: A): Randomized[A] = LazyList (a)

   def prob: Randomized[Probability] =
      LazyList.unfold[Double,Unit] (()) { _ =>
         Some (SecureRandom ().nextDouble -> ()) }


   def between (minInclusive: Int, maxExclusive: Int): Randomized[Int] =
      LazyList.unfold[Int,Unit] (()) { _ =>
         val r = SecureRandom ()
            .nextInt (maxExclusive - minInclusive) + minInclusive
         Some (r -> ())
      }


   def between (minInclusive: Double, maxExclusive: Double): Randomized[Double] =
      LazyList.unfold[Double,Unit] (()) { _ =>
         val r = SecureRandom ()
            .doubles (minInclusive, maxExclusive)
            .findAny
            .getAsDouble
         Some (r -> ())
      }


   /** Toss a coing biased towards true with probabilty 'bias' */
   def coin (bias: Probability): Randomized[Boolean] =
      LazyList.unfold[Boolean,Unit] (()) { _ =>
         val r = SecureRandom ().nextDouble <= bias
         Some (r -> ()) }

   def oneOf[A] (choices: A*): Randomized[A] =
      for i <- between (0, choices.size) yield choices (i)


   given randomizedIsMonad: cats.Monad[Randomized] =
      cats.instances.lazyList.catsStdInstancesForLazyList


   given canTestInRandomized: symsim.CanTestIn[Randomized] =
      new symsim.CanTestIn[Randomized] {

         def toProp (rProp: Randomized[Boolean]) =
            Prop.forAllNoShrink (toGen (rProp)) (identity[Boolean])

         private def rand_prefix[A] (ra: Randomized[A]): Int => A =
            (i: Int) => Try { ra (i) } getOrElse { rand_prefix (ra) (i/2) }

         // This is a nasty hack that costs as a lot on memory in tests (but
         // probably not in experiments).  Unfortunately, I do not see an easy way
         // to add a completely new generator for scalacheck that encapsulates
         // Randomized.  The Gen class appears to be sealed and pimping cannot add
         // state to objects?
         def toGen[A] (ra: Randomized[A]): Gen[A] =
            require (ra.nonEmpty)
            Gen.resultOf[Int,A] (this.rand_prefix (ra))
               (org.scalacheck.Arbitrary (Gen.chooseNum (0, 1000)))

      }
