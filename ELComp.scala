// ExpLang to StackM1 Compiler
//
// Usage: linux> scala ELComp <source file>
//
//
////////////////////////////////////////////////////////////////////////////////
// Name: Camilo Schaser-Hughes
// Class: CS558, Prog Languages
// Date: Oct 15, 2022
// Worked in a study group with Nick Giampietro, Aung Baw and 
// Rhitabrat Pokharel.  We went over some sudo code stuff, all of this code
// is my own tho.
////////////////////////////////////////////////////////////////////////////////
import ExpLang._
import StackM1._

object ELComp {
  var nextLabel: Int = 100

  case class CompileException(string: String) extends RuntimeException

  def compile(e: Expr): Program = e match {
    case True => Const(1)::Nil
    case False => Const(0)::Nil
    case Num(n) => Const(n)::Nil
    // implements the not equation by adding 1 and moding by 2.
    case Not(e) => {
      compile(e) ::: (
        Const(1) ::
        SAdd ::
        Const(2) ::
        Divrem ::
        Swap ::
        Pop::Nil
      )
    } // done
    // just does a bitwise and.
    case And(l,r) => {
      compile(l) :::
      compile(r) :::
      SAnd::Nil
      } // done
    // just does a bitwise or.
    case Or(l,r)  => {
      compile(l) ::: compile(r) ::: SOr::Nil
      } // done
    // compiles the left and right and then does the 
    // or the the two, then an and and subtracts the two
    case Xor(l,r) => {
      val left = compile(l)
      val right = compile(r)
      left ::: right ::: (SOr::Nil) ::: left ::: right ::: 
      (SAnd :: Const(-1) :: SMul :: SAdd::Nil)
      } // done
    // just does an add
    case Add(e1,e2) => {
      compile(e1) ::: compile(e2) ::: (SAdd::Nil)
      } // done
    // does a multiplication by -1 and then an add to sim the sub
    case Sub(e1,e2) => {
      compile(e1) ::: compile(e2) ::: (Const(-1) :: SMul :: SAdd::Nil)
      } // done
    // does a straight multiplication
    case Mul(e1,e2) => {
      compile(e1) ::: compile(e2) ::: (SMul::Nil)
      } // done
    // does a divrem and then pops the rem.
    case Div(e1,e2) => {
      compile(e1) ::: compile(e2) ::: (Divrem :: Pop::Nil)
      } // done
    // does a divrem, a swap and then pops the div
    case Rem(e1,e2) => {
      compile(e1) ::: compile(e2) ::: (Divrem :: Swap :: Pop::Nil)
      } // done
    // does a subtraction, sees if is above 0 and then jumps to label
    // to place a zero or a one there.
    case Gt(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Const(-1) :: SMul :: 
      SAdd :: Ifgt(lab1) :: Const(0) :: Goto(lab2) :: Label(lab1) :: 
      Const(1) :: Label(lab2)::Nil)
      } // done
    // same as above but with a swap.
    case Lt(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Swap :: Const(-1) :: 
      SMul :: SAdd :: Ifgt(lab1) :: Const(0) :: 
      Goto(lab2) :: Label(lab1) :: Const(1) :: Label(lab2)::Nil)
      } // done
    // same as gt but with an Ifz instead of Ifgt.
    case Eq(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Const(-1) :: 
      SMul :: SAdd :: Ifz(lab1) :: Const(0) :: 
      Goto(lab2) :: Label(lab1) :: Const(1) :: Label(lab2)::Nil)
      } // done
    // compiles the c and depending, jumps to a label and compiles
    // either t or f.
    case If(c,t,f) => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(c) ::: (Ifgt(lab1)::Nil) ::: compile(f) ::: (Goto(lab2)::Nil) ::: 
      (Label(lab1)::Nil) ::: compile(t) ::: (Label(lab2)::Nil)
      } // done
    case _ => throw CompileException("Illegal expr:" + e)
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ELParse(s)
    if (debug > 0) println("AST:    " + e)
    val p: Program = compile(e)
    if (debug > 0) println("Target: " + p)
    exec(p,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val v = apply(s,d)
      println(v)
    } catch {
      case ex: ParseException => println("Parser Error: " + ex.string)
      case ex: CompileException => println("Compile Error: " + ex.string)
    }
    ()
  }
}
//
