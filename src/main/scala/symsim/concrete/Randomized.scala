package symsim.concrete

/** A purely functional wrapping of scala.util.Random. Delegations. */
object Randomized {

  import cats.data.State
  import org.scalacheck.Prop

  /** Create a generator that always produces a. Used to create deterministic
    * values when a scheduler/randomized type is expected. TODO: this could
    * likely be moved to a super class for all schedulers.
    */
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


  // TODO: Car seems to have instances in breaking, perhaps we should move these
  implicit def randomizedIsMonad: cats.Monad[Randomized] =
    cats.data.IndexedStateT.catsDataMonadForIndexedStateT


  implicit val canTestInRandomized =
    new symsim.CanTestIn[Randomized] {
      def test (rProp: Randomized[Boolean]) =
          Prop.forAllNoShrink (rProp.toGen) (identity[Boolean])
    }

}



