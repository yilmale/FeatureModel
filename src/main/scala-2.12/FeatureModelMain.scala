/**
  * Created by yilmaz on 11/17/16.
  */

import algebra._
import algebra.MechanimComb._
import AlgebraCore._
import FOModel._


object FeatureModelMain extends App {

  object F1Algebra extends F1Generator
  object F2Algebra extends F2Generator
  object F3Algebra extends F3Generator

  object F1_F2Algebra extends MechanismMerge[Feature1,Feature2] {
      val alg1 = F1Algebra
      val alg2 = F2Algebra
      val lifter = LiftF1_F2
  }

  val o1:Feature1 = ExpAlgebra.exp(F1Algebra)
  val o2:Feature2 = ExpAlgebra.exp(F2Algebra)

  println("Expression: " + o2.print() + "\nevaluates to: " + o1.eval())

  val o3 = ExpAlgebra.exp(F1_F2Algebra)

  println("Eval: " + o3.eval() + "\nPrint: " + o3.print())

  val o4 = ExpAlgebra.exp(merge(LiftF1_F2,F1Algebra,F2Algebra))
  println("Eval Merge: " + o4.eval() + "\nPrint Merge: " + o4.print())


  val step2 = merge(LiftF1F2_F3, merge(LiftF1_F2, F1Algebra, F2Algebra), F3Algebra)

  val o5 = ExpAlgebra.exp(step2)
  println("Eval Double Merge: " + o5.eval() + "\nPrint Double Merge: " + o5.print() +
    "\nCount Double Merge: " + o5.count())

  FOMConstruct()
  evaluate(FOModel.fModel.subfeatures)



}
