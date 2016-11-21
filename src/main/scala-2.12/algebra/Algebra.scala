package algebra

/**
  * Created by yilmaz on 11/17/16.
  */

import scala.collection.mutable.HashMap
import scala.reflect._
import java.lang.reflect.Method
import java.lang.reflect.InvocationHandler
import java.lang.reflect.Proxy

trait Feature1 {
  def eval() : Int
}

trait Feature2 {
  def print() :String
}

trait Feature3 {
  def count() : Int
}


trait MechanismAlg[E] {
  def Lit(x:Int) : E
  def Add(e1:E, e2: E) : E
}

trait F1Generator extends MechanismAlg[Feature1] {
  def Lit(x : Int) : Feature1 = new Feature1 {
    def eval() : Int = x
  }

  def Add(e1: Feature1, e2: Feature1): Feature1 =  new Feature1 {
    def eval() : Int = {e1.eval() + e2.eval()}
  }
}



trait F2Generator extends MechanismAlg[Feature2] {
  def Lit(x : Int) : Feature2 = new Feature2 {
    def print() : String = x.toString()
  }

  def Add(e1: Feature2, e2: Feature2): Feature2 =  new Feature2 {
    def print() : String = {e1.print() + e2.print()}
  }
}

trait F3Generator extends MechanismAlg[Feature3] {
  def Lit(x : Int) : Feature3 = new Feature3 {
    def count() : Int = x
  }

  def Add(e1: Feature3, e2: Feature3): Feature3 =  new Feature3 {
    def count() : Int = {e1.count() + e2.count()}
  }
}



trait Lifter[A,B] {
  def lift(x : A, y : B ) : A with B
}

class MkLifter[A,B](f : (A,B) => A with B) extends Lifter[A,B] {
  def lift(x : A, y : B) : A with B = f(x,y)
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

object LiftF1_F3 extends Lifter[Feature1,Feature3] {
  def lift(x : Feature1, y : Feature3) = new Feature1 with Feature3 {
    def eval() = x.eval()
    def count() = y.count()
  }
}

object LiftF1F2_F3 extends Lifter[Feature1 with Feature2,Feature3] {
  def lift(x : Feature1 with Feature2, y : Feature3) = new Feature1 with Feature2 with Feature3 {
    def eval() = x.eval()
    def print() = x.print()
    def count() = y.count()
  }
}




trait MechanismEmpty extends MechanismAlg[Any] {
  def Lit(x : Int) : Any = new Object()
  def Add(e1: Any, e2: Any) : Any = new Object()
}

object MechEmpty extends MechanismEmpty

class LiftDecorate[A](action : A => A) extends Lifter[A,Any] {
  def lift(x : A, y : Any) = action(x)
}

trait ExpEmpty extends MechanismAlg[Any] {
  def Lit(x : Int) : Any = new Object()
  def Add(e1 : Any, e2 : Any) : Any = new Object ()
}

object ExpEmpty extends ExpEmpty

trait ExpDecorate[A] extends MechanismMerge[A,Any] {
  val alg2 = ExpEmpty
  val lifter = new LiftDecorate(action)
  def action(x : A) : A
}



trait Algebra[F[_]] {
  def createInstance[A](ih : InvocationHandler)(implicit m : ClassTag[A]) : A = {
    Proxy.newProxyInstance(m.runtimeClass.getClassLoader, Array(m.runtimeClass),ih).asInstanceOf[A]
  }
  // Basic combinators
  def merge[A,B](lifter : Lifter[A,B], a1 : F[A], a2 : F[B])(implicit m : ClassTag[F[A with B]]) : F[A with B] =
  createInstance(new InvocationHandler() {
    def invoke(proxy : Object, method : Method, args : Array[Object]) = {
      val a = method.invoke(a1,args : _*)
      val b = method.invoke(a2,args : _*)
      lifter.lift(a.asInstanceOf[A],b.asInstanceOf[B]).asInstanceOf[Object]
    }})

  def empty(implicit m : ClassTag[F[Any]]) : F[Any] =
    createInstance(new InvocationHandler() {
      def invoke(proxy : Object, method : Method, args : Array[Object]) = new Object()
    })

  // Derived combinator(s)
  def delegate[A,B](x : A, y : B)(implicit m : ClassTag[A with B]) : A with B = createInstance(new InvocationHandler() {
    def invoke(proxy : Object, method : Method, args : Array[Object]) : Object = {
      try {
        method.invoke(x, args : _*)
      } catch {
        case e : IllegalArgumentException => method.invoke(y, args : _*)
      }
    }
  })

  def combine[A,B](alg1 : F[A], alg2 : F[B])(implicit m1 : ClassTag[F[A with B]], m2 : ClassTag[A with B]) : F[A with B] =
    merge[A,B](new MkLifter(delegate[A,B] _), alg1, alg2)

  def decorate[A](parent : F[A], action : A => A)(implicit m1 : ClassTag[F[A]], m2 : ClassTag[F[Any]]) : F[A] =
    merge[A,Any](new LiftDecorate(action),parent,empty)
}




object MechanimComb extends Algebra[MechanismAlg]



object ExpAlgebra {
  def exp[E](f: MechanismAlg[E]) : E =  f.Add(f.Lit(5),f.Add(f.Lit(6),f.Lit(6)))
}
