package algebra

/**
  * Created by yilmaz on 11/17/16.
  */


trait Feature1 {
  def eval() : Int
}

trait Feature2 {
  def print() :String
}


trait MechanismAlg[E] {
  def Lit(x:Int) : E
  def Add(e1:E, e2: E) : E
}

trait F1 extends MechanismAlg[Feature1] {
  def Lit(x : Int) : Feature1 = new Feature1 {
    def eval() : Int = x
  }

  def Add(e1: Feature1, e2: Feature1): Feature1 =  new Feature1 {
    def eval() : Int = {e1.eval() + e2.eval()}
  }
}



trait F2 extends MechanismAlg[Feature2] {
  def Lit(x : Int) : Feature2 = new Feature2 {
    def print() : String = x.toString()
  }

  def Add(e1: Feature2, e2: Feature2): Feature2 =  new Feature2 {
    def print() : String = {e1.print() + e2.print()}
  }
}

trait Lifter[A,B] {
  def lift(x : A, y : B) : A with B
}

trait MechanismMerge[A,B] extends MechanismAlg[A with B] {
  val lifter : Lifter[A,B]
  val alg1 : MechanismAlg[A]
  val alg2 : MechanismAlg[B]

  def Lit(x : Int) : A with B = {
    lifter.lift(alg1.Lit(x), alg2.Lit(x))
  }

  def Add(e1: A with B, e2: A with B) : A with B = {
    lifter.lift(alg1.Add(e1, e2), alg2.Add(e1, e2))
  }

}

object LiftF1_F2 extends Lifter[Feature1,Feature2] {
  def lift(x : Feature1, y : Feature2) = new Feature1 with Feature2 {
    def eval() = x.eval()
    def print() = y.print()
  }
}


object Algebra {
  def exp[E](f: MechanismAlg[E]) : E =  f.Add(f.Lit(5),f.Add(f.Lit(6),f.Lit(6)))
}
