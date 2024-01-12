package symsim
package examples.concrete.simplebandit

import symsim.concrete.Randomized2

val BanditObjConst = new Bandit (List (
  Randomized2.const (-0.2),
  Randomized2.const ( 0.2),
  Randomized2.const ( 0.4),
  Randomized2.const (-0.1),
  Randomized2.const ( 0.3),
  Randomized2.const ( 0.8),
  Randomized2.const ( 0.6),
  Randomized2.const (-0.6),
  Randomized2.const ( 0.0),
  Randomized2.const ( 0.7)))
