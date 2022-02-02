package symsim
package examples.concrete.simplebandit

import symsim.concrete.Randomized

val BanditObjConst = new Bandit(List(Randomized.const(-0.2),
		Randomized.const(0.2),
		Randomized.const(0.4),
		Randomized.const(-0.1),
		Randomized.const(0.3),
		Randomized.const(0.8),
		Randomized.const(0.6),
		Randomized.const(-0.6),
		Randomized.const(0.0),
		Randomized.const(0.7)))
