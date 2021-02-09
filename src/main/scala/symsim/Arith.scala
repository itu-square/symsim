package symsim

// spire.math.Rational might be an interesting addition to this family to help
// reasoning better

import scala.math.Numeric
import cats.Eq

/**
 * A variation on scala.math.Numeric, where we control what operations are
 * required.  We only require operations used in SARSA now.  The idea is that
 * We can then add the instance of Arith for symbolic representations.
 */

trait Arith[A] extends Eq[A] {

  def times (a1: A, a2: A): A

  def minus (a1: A, a2: A): A

  def plus (a1: A, a2: A): A

  def doubleTimes (d: Double, a: A): A
}


object Arith {

  def Arith[A: Arith]: Arith[A] =
    implicitly[Arith[A]]

  implicit class arithOps[A: Arith] (a: A) {

    def * (b: A) = Arith[A].times (a,b)

    def - (b: A) = Arith[A].minus (a,b)

    def + (b: A) = Arith[A].plus (a,b)

  }

   // TODO: instances should eventually be moved to symsim.instances
  implicit class doubleOps (d: Double) {

    def  times[A: Arith] (a: A): A =
      Arith[A].doubleTimes (d,a)

    def * [A: Arith] (a: A): A =
      times[A] (a)
  }


  // Instances for standard types

  implicit object arithDouble extends Arith[Double] {

      def times (x: Double, y: Double): Double =
        x * y

      def plus (x: Double, y: Double): Double =
        x + y

      def minus (x: Double, y: Double): Double =
        x - y

      def doubleTimes (d: Double, a: Double): Double =
        d * a

      def eqv (x: Double, y: Double): Boolean =
        x == y
  }

}
