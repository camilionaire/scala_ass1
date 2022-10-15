// ExpLang to StackM1 Compiler
//
// Usage: linux> scala ELComp <source file>
//
//
import ExpLang._
import StackM1._

object ELComp {
  var nextLabel: Int = 100

  case class CompileException(string: String) extends RuntimeException

  def compile(e: Expr): Program = e match {
    case True => Const(1)::Nil
    case False => Const(0)::Nil
    case Num(n) => Const(n)::Nil
    case Not(e) => {
      // compile(e) ::: (Const(1)::Nil) ::: (SAdd::Nil) ::: (Divrem::Nil) ::: (Swap::Nil) ::: (Pop::Nil)
      compile(e) ::: (
        Const(1) ::
        SAdd ::
        Const(2) ::
        Divrem ::
        Swap ::
        Pop::Nil
      )
    } // ... need code ...
    case And(l,r) => {
      compile(l) :::
      compile(r) :::
      SAnd::Nil
      } // ... need code ...
    case Or(l,r)  => {
      compile(l) ::: compile(r) ::: SOr::Nil
      } // ... need code ...
    case Xor(l,r) => {
      val left = compile(l)
      val right = compile(r)
      left ::: right ::: (SOr::Nil) ::: left ::: right ::: (SAnd::Nil) ::: (Const(-1)::Nil) ::: (SMul::Nil) ::: (SAdd::Nil)
      } // ... need code ...
    // case Add(e1,e2) => // ... need code ...
    // case Sub(e1,e2) => // ... need code ...
    // case Mul(e1,e2) => // ... need code ...
    // case Div(e1,e2) => // ... need code ...
    // case Rem(e1,e2) => // ... need code ...
    // case Gt(e1,e2)  => // ... need code ...
    // case Lt(e1,e2)  => // ... need code ...
    // case Eq(e1,e2)  => // ... need code ...
    // case If(c,t,f) => // ... need code ...
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
