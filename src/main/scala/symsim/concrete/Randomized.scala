package symsim.concrete


// TODO: This import and the propInScheduler function seem to be in a wrong
// module (shouldn't this be somewhere on the testing side?)
import org.scalacheck.Prop
import cats.Eq
import cats.data.State

/** A purely functional wrapping of scala.util.Random. Delegations. */
object Randomized {


  def const[A] (a: A): Randomized[A] =
    State { r => (r, a) }


  def prob: Randomized[Probability] =
    State { r => (r, r.nextDouble ()) }


  def between (minInclusive: Int, maxExclusive: Int): Randomized[Int] =
    State { r => (r, r.between (minInclusive, maxExclusive)) }


  def between (minInclusive: Double, maxExclusive: Double): Randomized[Double] =
    State { r => (r, r.between (minInclusive, maxExclusive)) }


  def coin (bias: Probability): Randomized[Boolean] =
    for { toss <- prob }
    yield toss <= bias


  def oneOf[A] (choices: Seq[A]): Randomized[A] =
    for { i <- between (0, choices.size) }
    yield choices (i)


  implicit def propInScheduler[A] (implicit ev: A => Prop)
    : Randomized[A] => Prop =
    { ra: Randomized[A] => Prop.forAllNoShrink (ra.toGen) { a => ev(a) } }

}



