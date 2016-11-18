/**
  * Created by yilmaz on 11/17/16.
  */

import algebra._

object FeatureModelMain extends App {

   object F1Algebra extends F1
   object F2Algebra extends F2

   object F1_F2Algebra extends MechanismMerge[Feature1,Feature2] {
      val alg1 = F1Algebra
      val alg2 = F2Algebra
      val lifter = LiftF1_F2
   }

   val o1:Feature1 = Algebra.exp(F1Algebra)
   val o2:Feature2 = Algebra.exp(F2Algebra)

   println("Expression: " + o2.print() + "\nevaluates to: " + o1.eval())

   val o3 = Algebra.exp(F1_F2Algebra)

   println("Eval: " + o3.eval() + "\nPrint: " + o3.print())

}
