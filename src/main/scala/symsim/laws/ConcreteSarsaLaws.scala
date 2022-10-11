package symsim
package laws


import symsim.concrete.ConcreteSarsa
import cats.kernel.laws.*
import cats.kernel.laws.discipline.*
import cats.kernel.BoundedEnumerable
import symsim.concrete.Randomized

import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.*
import org.scalacheck.Prop._

import org.scalacheck.Gen

import symsim.CanTestIn.*
import symsim.Arith.*

// import me.shadaj.scalapy.py
// import me.shadaj.scalapy.py.SeqConverters
// import me.shadaj.scalapy.py.writableSeqElem
// import me.shadaj.scalapy.py
// import jep.Jep



// import org.apache.spark.SparkContext
// import org.apache.spark.mllib.linalg._
// import org.apache.spark.mllib.regression.LabeledPoint
// import org.apache.spark.mllib.stat.Statistics._

/**
 * Laws that have to be obeyed by any refinement of symsim.ConcreetSarsa
 *
 * TODO: Why is this just for Concrete? Does it have to?
 */
case class ConcreteSarsaLaws[State, FiniteState, Action]
   (sarsa: ConcreteSarsa[State, FiniteState, Action])
   extends org.typelevel.discipline.Laws:

   import sarsa.agent.instances.given

   def isStateTotal (q: sarsa.Q): Boolean =
     q.keySet == sarsa.agent.instances.allObservableStates.toSet

   def isActionTotal (q: sarsa.Q): Boolean =
     q.values.forall { _.keySet == sarsa.agent.instances.allActions.toSet}

   //val generateStates : Gen[State]= sarsa.agent.instances.arbitraryState

   val actions = Gen.oneOf (sarsa.agent.instances.enumAction.membersAscending)
   val states = sarsa.agent.instances.arbitraryState.arbitrary
   val sarsa1= ConcreteSarsa(sarsa.agent,0.1,0.1,0.09,5000)



   val laws: RuleSet = new SimpleRuleSet (
      "concreteSarsa",
            "probability of choosing best action is 1-epsilon" ->
            forAllNoShrink(sarsa.genVF, actions){(q:sarsa.Q,a_t)=>
                  if(!q.isEmpty && !q.values.toSet.contains(Map()))
                          var numberOfBest=0
                          var numberOfRandom=0
                          val initials = Randomized.repeat (sarsa.agent.initialize).take (sarsa.episodes)
                          val exps=Randomized.repeat (for
                                s_t<-initials.filter(
                                  q.keySet.contains.compose(sarsa.agent.discretize)(_)
                                )
                          yield sarsa.chooseAction(q)(s_t).head==sarsa.bestAction(q)(s_t)). take(sarsa.episodes)
                          //for(x<-exps){print(x)}
                          if(exps.contains(true))
                             numberOfBest=exps.groupBy(identity).map(x => (x._1, x._2.size))(true)
                          if(exps.contains(false))
                             numberOfRandom=exps.groupBy(identity).map(x => (x._1, x._2.size))(false)

                          val probUpperbound=sarsa.epsilon+0.1
                          val probLowerBound=sarsa.epsilon-0.1
                        //  print(np.arange(15).reshape(3, 5))
                          //print(numberOfRandom.toFloat/(numberOfRandom+numberOfBest))
                          numberOfRandom.toFloat/(numberOfRandom+numberOfBest)<=probUpperbound
                          //&&
                          //numberOfRandom.toFloat/(numberOfRandom+numberOfBest)>= probLowerBound

                          // val goodnessOfFitTestResult = Statistics.chiSqTest(exps)
                          // println(goodnessOfFitTestResult)

                  else
                      true
              },
    )














    // "new probability of choosing best action is 1-epsilon" ->
    // forAllNoShrink(sarsa.genQ,actions){(q:sarsa.Q,a_t)=>
    //       if(!q.isEmpty && !q.values.toSet.contains(Map()))
    //               var numberOfBest=0
    //               var numberOfRandom=0
    //               val initials = Randomized.repeat (sarsa.agent.initialize).take (sarsa.episodes)
    //               val exps=Randomized.repeat (for
    //                     s_t<-initials.filter(
    //                       q.keySet.contains.compose(sarsa.agent.discretize)(_)
    //                     )
    //                     next<-sarsa.learningEpoch(q,s_t,a_t)
    //               yield next._3==sarsa.bestAction(q)(s_t)). take(sarsa.episodes)
    //               for(x<-exps){print(x)}
    //               if(exps.contains(true))
    //                  numberOfBest=exps.groupBy(identity).map(x => (x._1, x._2.size))(true)
    //               if(exps.contains(false))
    //                  numberOfRandom=exps.groupBy(identity).map(x => (x._1, x._2.size))(false)
    //
    //
    //               val probUpperbound=sarsa.epsilon
    //               val probLowerBound=sarsa.epsilon-0.0001
    //               print(numberOfRandom.toFloat/(numberOfRandom+numberOfBest))
    //               numberOfRandom.toFloat/(numberOfRandom+numberOfBest)<=probUpperbound
    //               //&&
    //             //  numberOfRandom.toFloat/(numberOfRandom+numberOfBest)>= probLowerBound
    //               true
    //
    //       else
    //           true
    //   },
