// ExpLang Interpreter
//
// Usage: linux> scala ELInterp <source file>
//
//
import ExpLang._

object ELInterp {
  case class InterpException(string: String) extends RuntimeException

  def interp(e:Expr): Either[Boolean,Int] = e match {
    case True => Left(true)
    case False => Left(false)
    case Num(n) => Right(n)
    case Not(e)  => {
      interp(e) match {
        case Left(true) => Left(false)
        case Left(false) => Left(true)
        case _ => throw InterpException("Can only not a bool: " + e)
      }
      }// ... need code ...
    case And(l,r) => {
      val left = interp(l)
      val right = interp(r)
      val cl = left match {
        case Left
      }
      

      }// ... need code ...
    // case Or(l,r)  => // ... need code ...
    // case Xor(l,r) => // ... need code ...
    // case Add(l,r) => // ... need code ...
    // case Sub(l,r) => // ... need code ...
    // case Mul(l,r) => // ... need code ...
    // case Div(l,r) => // ... need code ...
    // case Rem(l,r) => // ... need code ...
    // case Lt(l,r) => // ... need code ...
    // case Gt(l,r) => // ... need code ...
    case Eq(l,r) => {
      
      }// ... need code ...
    // case If(c,l,r) => // ... need code ... 
    case _ => throw InterpException("Illegal expr:" + e)
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ELParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val v = apply(s,d)
      v match {
        case Left(b)  => printf(b+"\n")
        case Right(i) => printf(i+"\n")
      }
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
