package symsim
package examples.concrete.simplemaze

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalatest.*
import prop.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalatest.prop.Whenever
import org.scalatest.*
import org.scalacheck.Prop.{exists, forAll, forAllNoShrink, propBoolean}
import examples.concrete.simplemaze.MazeState
import examples.concrete.simplemaze.Maze
import symsim.concrete.{ConcreteSarsa, Randomized}

/** Sanity tests for Randomized as a Scheduler */
class MazeSpec
  extends org.scalatest.freespec.AnyFreeSpec,
    org.scalatestplus.scalacheck.Checkers:

  "Sanity checks for symsim.concrete.breaking" - {

    // Generators of test data
    val xs = Gen.oneOf [Int](1, 2, 3, 4)
    val ys = Gen.oneOf [Int](1,2, 3)
    val actions = Gen.oneOf (Maze.instances.enumAction.membersAscending)
    val states = Gen.oneOf [(Int, Int)]((1, 1), (1, 2), (1, 3), (2, 1), (2, 3),
      (3, 1), (3, 2), (3, 3), (4, 1), (4, 2), (4, 3))


    // Tests

    "Agent near to left wall, cannot go left" in check {
      forAll (ys) { y =>
        for (s1, r) <- Maze.step (1, y) (Left)
        yield s1._1 == 1
      }
    }

    "Agent near to right wall, cannot go right" in check {
      forAll(ys) { y =>
        for (s1, r) <- Maze.step(4, y) (Right)
        yield s1._1 == 4
      }
    }

    "Agent near to up wall, cannot go up" in check {
      forAll(xs) { x =>
        for (s1, r) <- Maze.step(x, 3) (Up)
        yield s1._2 == 3
      }
    }

    "Agent near to down wall, cannot go down" in check {
      forAll(xs) { x =>
        for (s1, r) <- Maze.step(x, 1) (Down)
        yield s1._2 == 1
      }
    }

    "Agent cannot go into block" in check {
      forAllNoShrink(states, actions) { (s, a) =>
        for (s1, r) <- Maze.step(s) (a)
        yield s1 != (2, 2)
      }
    }

    "checking optimal policy" in check {
      val sarsa = ConcreteSarsa(Maze, 0.1, 1, 0.05, 100000)
      forAllNoShrink(states) { s =>
        val Q = sarsa.learningEpisode (sarsa.initialize, s).head
        val optPolicy = sarsa.bestAction (Q)(s)
        optPolicy != Down
      }
    }
  }
