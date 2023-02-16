package symsim

// spire.math.Rational might be an interesting alternative here

import scala.annotation.targetName
import cats.Eq

/**
 * A variation on scala.math.Numeric, where we control what operations are
 * required.  We only require operations used in SARSA now.  The idea is that
 * We can then add the instance of Arith for symbolic representations.
 */

trait Arith[A] extends Eq[A]:
  def times (a1: A, a2: A): A
  def minus (a1: A, a2: A): A
  def plus (a1: A, a2: A): A
  def doubleTimes (d: Double, a: A): A
  def zero: A


object Arith:

  def arith[A: Arith]: Arith[A] = summon[Arith[A]]

  extension [A: Arith] (a: A)
    def * (b: A): A = arith[A].times (a,b)
    def - (b: A): A = arith[A].minus (a,b)
    def + (b: A): A = arith[A].plus (a,b)

  extension (d: Double)
    @targetName ("multiply")
    def *[A: Arith] (a: A): A = d.times[A] (a)
    def times[A: Arith] (a: A): A = arith[A].doubleTimes (d, a)

  extension [A] (l: List[A]) (using arithA: Arith[A])
    def arithSum = l.foldLeft (arithA.zero) (arithA.plus)

  /* Instances for standard types */

  given arithDouble: Arith[Double] = new Arith[Double]:
    def times (x: Double, y: Double): Double = x * y
    def plus (x: Double, y: Double): Double = x + y
    def minus (x: Double, y: Double): Double = x - y
    def doubleTimes (d: Double, a: Double): Double = d * a
    def eqv (x: Double, y: Double): Boolean = x == y
    def zero: Double = 0.0
