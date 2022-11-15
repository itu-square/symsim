package symsim
package examples.concrete.windygrid

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalatest.*
import prop.*
import org.scalacheck.Gen
import org.scalacheck.Prop.{exists, forAll, forAllNoShrink, propBoolean}
import examples.concrete.windygrid.GridState
import examples.concrete.windygrid.WindyGrid
import symsim.concrete.{ConcreteSarsa, Randomized}

/** Sanity tests for Randomized as a Scheduler */
class WindyGridSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  given PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sanity checks for symsim.concrete.windygrid" - {

    // Generators of test data
    val xs = Gen.choose [Int](1, 10)
    val ys = Gen.choose [Int](1, 7)
    val actions = Gen.oneOf (WindyGrid.instances.enumAction.membersAscending)

    // Tests

    "Up and Down will never affect the x value" in check {
      forAll (xs, ys) { (x, y) =>
        for
          (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.U)
          (s2, r) <- WindyGrid.step (GridState (x, y)) (GridAction.D)
        yield s1._1 == x && s2._1 == x
      }
    }

    "When there is wind, R will affect both x and y unless in the y's upper bound" in check {
      forAll(xs, ys) { (x, y) =>
        for
          (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.R)
        yield (x >= 4 && x <= 8 && y <= 6) ==> (s1._1 != x && s1._2 != y)
      }
    }

    "When there is wind, L will affect both x and y unless in the y's upper bound" in check {
      forAll(xs, ys) { (x, y) =>
        for
          (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.L)
        yield (x >= 4 && x <= 8 && y <= 6) ==> (s1._1 != x && s1._2 != y)
      }
    }

    "Q-table values are non-positive" in check {
      val sarsa = ConcreteSarsa(WindyGrid, 0.1, 1, 0.05, 100000)
      forAll (xs, ys, actions) { (x, y, a) =>
        for
          Q <- sarsa.learningEpisode (sarsa.initialize, GridState(x, y))
        yield Q (GridState(x, y)) (a) <= 0
      }
    }
  }
