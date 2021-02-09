package symsim.concrete

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

}



