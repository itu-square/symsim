package symsim

import cats.kernel.BoundedEnumerable
import cats.kernel.Order

// Something like this should be easily doable in cats, but it does not seem so.
case class BoundedEnumerableFromList[A] (as: A*)
  extends BoundedEnumerable[A]:

  require (as.distinct.size == as.size, s"as='$as' contains duplicates")
  require (as.size > 0, s"as.size = ${as.size} but should be positive")

  private val idx: Map[A,Int] = as.zipWithIndex.toMap
  private val idxR: Map[Int,A] = idx map { _.swap }

  val minBound = as.head
  val maxBound = as.last

  def order: Order[A] =  new Order[A] {
    def compare (x: A, y: A): Int = idx (x) - idx (y)
  }

  def partialNext (a: A): Option[A] =
    require (as contains a)
    idxR.get (idx (a) + 1)

  def partialPrevious (a: A): Option[A] =
    require (as contains a)
    idxR.get (idx (a) - 1)

  object Laws

    // TODO getting members ascending should give 'a'
    // TODO getting members descending should give 'a' reversed
