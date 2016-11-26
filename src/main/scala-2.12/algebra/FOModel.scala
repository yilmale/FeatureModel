package algebra

import scala.collection.mutable

/**
  * Created by Levent Yilmaz on 11/23/2016.
  */

abstract class FOM {
  val subfeatures : List[FOM]
}
case class FModel(name:String,subfeatures: List[FOM]) extends FOM
case class Feature(name:String, subfeatures:List[FOM]=Nil) extends FOM
case class And(subfeatures:List[FOM]) extends FOM
case class Or(subfeatures:List[FOM]) extends FOM
case class Xor(subfeatures:List[FOM]) extends FOM
case class Optional(subfeatures:List[FOM]) extends FOM

object FOModel {

   val fModel = FModel("X",
     List(
       And(List(Or(List(Feature("A"),Feature("D"),Feature("E"))),Feature("B"))),
       Feature("Z"))
   )


    def FOMConstruct(): Unit = {
      println(fModel.name)
      println(fModel)
    }

    def evaluate(fmodel: List[FOM]): Unit = {
      println("Model is " + fmodel)
      fmodel match {
        case h::t =>
          println(h)
          h match {
            case And(_) =>
              println("And operator")
              for (x <- h.subfeatures)
                evaluate(List(x))
            case Or(_) =>
              println("Or operator")
              for (x <- h.subfeatures)
                evaluate(List(x))
            case Feature(_,_) =>
              println("Plain feature")
              for (x <- h.subfeatures)
                evaluate(List(x))
            case Xor(_) =>
              println("Xor operator")
              for (x <- h.subfeatures)
                evaluate(List(x))
            case _ => println("Another operator")
          }

          println(t)
        case nil =>
          println("Null list")
        case _ => println("Did not match")
      }
    }




}
