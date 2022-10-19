package symsim

import scala.annotation.targetName

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAllNoShrink

/** A simple type class that defines contexts (schedulers) in which we can test
 *  with scalacheck
 */
trait CanTestIn[F[_]]:

  /** Allows to test whether all scheduled Boolean values are true. You can use
    * this to convert the scheduled value 'x' using a predicate 'p' (for example
    * map the predicate) and this way create a scheduled Boolean value that can
    * be converted to Prop and used in a test/law. This is quite concise, but
    * the created law does not look very much as a logical specification because
    * the quantifier is hidden in the implementation of test.
    */
  @targetName ("toPropBoolean")
  def toProp (fProp: F[Boolean]): Prop

  @targetName ("toPropProp")
  def toProp (fProp: F[Prop]): Prop = 
    forAllNoShrink (toGen (fProp)) { identity[Prop] }

  /** This is an alternative to 'test' that can be used to turn a scheduled
    * value into a generator.  Then this generator can be fed into a regular
    * scalacheck property (Prop) with `forAll (this.toGen) { x => p (x) }`. This
    * makes the properties look a bit more explicit, at the cost that a reader
    * may be falsely assuming that we are testing on random values, but we are
    * testing on scheduled values only.
    */
  def toGen[A] (fa: => F[A]): Gen[A]


object CanTestIn:

  /** Allow to use testIn[F] to access the given instance of
    * CanTestIn for F.
    */
  def testIn[F[_]: CanTestIn] = summon[CanTestIn[F]]

  extension [F[_]: CanTestIn] (fb: F[Boolean])
    def toProp: Prop = testIn[F].toProp (fb)

  extension [F[_]: CanTestIn, A] (fa: F[A])
    def toGen: Gen[A] = testIn[F].toGen (fa)

  given ConvertBooleanToProp[F[_]: CanTestIn]: Conversion [F[Boolean], Prop] with
    def apply (fb: F[Boolean]): Prop = 
      testIn[F].toProp (fb)

  given ConvertPropToProp[F[_]: CanTestIn]: Conversion [F[Prop], Prop] with
    def apply (fp: F[Prop]): Prop = 
      testIn[F].toProp (fp)
