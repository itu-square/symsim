package symsim.examples.concrete.simplemaze

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

import Maze.instances.{arbitraryState, arbitraryAction}

// Eliminate the warning on MazeSpec, until scalacheck makes Properties open
import scala.language.adhocExtensions

object MazeSpec
  extends org.scalacheck.Properties ("simplemaze.Maze"):

  property ("Agent near to left wall, cannot go left") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (1, s._2, s._3) (Left)
      yield s1._1 == 1
    }

  property ("Agent near to right wall, cannot go right") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (4, s._2, s._3) (Right)
      yield s1._1 == 4
    }

  property ("Agent near to up wall, cannot go up") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (s._1, 3, s._3) (Up)
      yield s1._2 == 3
    }

  property ("Agent near to down wall, cannot go down") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (s._1, 1, s._3) (Down)
      yield s1._2 == 1
    }

  property ("Agent cannot go into block") =
    forAll { (s: MazeState, a: MazeAction) =>
      for (s1, r) <- Maze.step (s) (a)
      yield (s1._1, s._2) != (2, 2)
    }
