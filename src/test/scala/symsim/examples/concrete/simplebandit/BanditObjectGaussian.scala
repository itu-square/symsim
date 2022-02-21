package symsim
package examples.concrete.simplebandit

import symsim.concrete.Randomized

val BanditObjGaussian = new Bandit(List(Randomized.gaussian(-0.2, 1),
		Randomized.gaussian(0.2, 1),
		Randomized.gaussian(0.4, 1),
		Randomized.gaussian(-0.1, 1),
		Randomized.gaussian(0.3, 1),
		Randomized.gaussian(0.8, 1),
		Randomized.gaussian(0.6, 1),
		Randomized.gaussian(-0.6, 1),
		Randomized.gaussian(0.0, 1),
		Randomized.gaussian(0.7, 1)))
