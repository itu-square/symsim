package symsim.concrete

import cats.data.State
import org.scalacheck.{Gen, Prop}

import scala.jdk.StreamConverters._
import scala.util.Try

import java.security.SecureRandom

/** A purely functional wrapping of scala.util.Random */
type Randomized[+A] = LazyList[A]

type Probability = Double

/** A purely functional wrapping of java.security.SecureRandom.  Presents
  * generators as possibly finite streams (a source that dries out), which can
  * be extended to continue producing infinitely with repeat (infinite).
  * There is some lack of referential transparency in this switch (repeat).
  *
  * Randomized should be a non-branching scheduler.  For the infrastructure to
  * work, all our schedulers need to be finitely branching.
  **/
object Randomized:

   /** Create a generator that always a. Used to create deterministic
     * values when a scheduler/randomized type is expected.
     */
   def const[A] (a: =>A): Randomized[A] =
     LazyList (a)

   def prob: Randomized[Probability] =
      LazyList (SecureRandom ().nextDouble)


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

   /** Toss a coing biased towards true with probabilty 'bias' */
   def coin (bias: Probability): Randomized[Boolean] =
      LazyList (SecureRandom ().nextDouble <= bias)

   def oneOf[A] (choices: A*): Randomized[A] =
      between (0, choices.size)
         .map { i => choices (i) }

   def repeat[A] (ra: =>Randomized[A]): Randomized[A] =
      LazyList.continually (ra).flatten

   given randomizedIsMonad: cats.Monad[Randomized] =
      cats.instances.lazyList.catsStdInstancesForLazyList

   given randomizedIsFoldable: cats.Foldable[Randomized] =
      cats.instances.lazyList.catsStdInstancesForLazyList

   given canTestInRandomized: symsim.CanTestIn[Randomized] =
      new symsim.CanTestIn[Randomized] {

         def toProp (rProp: Randomized[Boolean]) =
            Prop.forAllNoShrink (toGen (rProp)) (identity[Boolean])

         // This is a nasty hack that costs as a lot on memory in tests (but
         // probably not in experiments).  Unfortunately, I do not see an easy way
         // to add a completely new generator for scalacheck that encapsulates
         // Randomized.  The Gen class appears to be sealed and pimping cannot add
         // state to objects?
         def toGen[A] (ra: Randomized[A]): Gen[A] =
            require (ra.nonEmpty)
            val stream = repeat (ra)
            Gen.choose(0, 1000)
               .map { i => stream (i) }

      }
