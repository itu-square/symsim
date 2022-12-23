package symsim.examples.concrete.simplemaze

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

import Maze.instances.{arbitraryState, arbitraryAction}

// Eliminate the warning on MazeSpec, until scalacheck makes Properties open
import scala.language.adhocExtensions

class MazeSpec
  extends org.scalacheck.Properties ("simplemaze.Maze"):

  property ("Agent near to left wall, cannot go left") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (1, s.y) (Left)
      yield s1._1 == 1
    }

  property ("Agent near to right wall, cannot go right") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (4, s.y) (Right)
      yield s1._1 == 4
    }

  property ("Agent near to up wall, cannot go up") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (s.x, 3) (Up)
      yield s1._2 == 3
    }

  property ("Agent near to down wall, cannot go down") =
    forAll { (s: MazeState) =>
      for (s1, r) <- Maze.step (s.x, 1) (Down)
      yield s1._2 == 1
    }

  property ("Agent cannot go into block") =
    forAll { (s: MazeState, a: MazeAction) =>
      for (s1, r) <- Maze.step (s) (a)
      yield s1 != (2, 2)
    }
