package symsim.concrete

import cats.data.State
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

import scala.jdk.StreamConverters._

/** A purely functional wrapping of scala.util.Random */
type Randomized[A] = LazyList[A]

type Probability = Double

/** A purely functional wrapping of scala.util.Random. Delegations. */
object Randomized:

  /** Create a generator that always produces a. Used to create deterministic
    * values when a scheduler/randomized type is expected. TODO: this could
    * likely be moved to a super class for all schedulers.
    */
  def const[A] (a: A): Randomized[A] = LazyList.continually (a)


  def prob: Randomized[Probability] =
    java.security.SecureRandom ().doubles.toScala(LazyList)


  def between (minInclusive: Int, maxExclusive: Int): Randomized[Int] =
    java.security.SecureRandom ()
      .ints (minInclusive, maxExclusive).toScala (LazyList)


  def between (minInclusive: Double, maxExclusive: Double): Randomized[Double] =
    java.security.SecureRandom ()
      .doubles (minInclusive, maxExclusive).toScala (LazyList)


  /** Toss a coing biased towards true with probabilty 'bias' */
  def coin (bias: Probability): Randomized[Boolean] =
    for toss <- prob yield toss <= bias


  def oneOf[A] (choices: A*): Randomized[A] =
    for i <- between (0, choices.size) yield choices (i)


  given randomizedIsMonad: cats.Monad[Randomized] =
    cats.instances.lazyList.catsStdInstancesForLazyList


  given canTestInRandomized: symsim.CanTestIn[Randomized] =
    new symsim.CanTestIn[Randomized] {

      def toProp (rProp: Randomized[Boolean]) =
          Prop.forAllNoShrink (toGen (rProp)) (identity[Boolean])

      def toGen[A] (ra: Randomized[A]): Gen[A] =
        require (ra.nonEmpty)
        Gen.resultOf[Int,A] (n => ra (n % 1000))
          (org.scalacheck.Arbitrary (Gen.chooseNum(0, Int.MaxValue)))

    }
