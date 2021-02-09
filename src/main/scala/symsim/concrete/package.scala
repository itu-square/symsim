package symsim

package object concrete {

  /** A purely functional wrapping of scala.util.Random */

  type Randomized[A] = cats.data.State[scala.util.Random,A]

  type Probability = Double
  type Seed = Long

}
