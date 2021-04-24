package symsim



package object concrete:

  type Probability = Double
  type Seed = Long

  /** A purely functional wrapping of scala.util.Random */
  type Randomized[A] = cats.data.State[scala.util.Random,A]
