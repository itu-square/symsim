package symsim.examples.concrete.simplebandit

import symsim.concrete.Randomized2

val BanditObjGaussian: Bandit = new Bandit (List (
  Randomized2.gaussian (-0.2, 1),
  Randomized2.gaussian ( 0.2, 1),
  Randomized2.gaussian ( 0.4, 1),
  Randomized2.gaussian (-0.1, 1),
  Randomized2.gaussian ( 0.3, 1),
  Randomized2.gaussian ( 0.8, 1),
  Randomized2.gaussian ( 0.6, 1),
  Randomized2.gaussian (-0.6, 1),
  Randomized2.gaussian ( 0.0, 1),
  Randomized2.gaussian ( 0.7, 1)))
