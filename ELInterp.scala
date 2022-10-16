// ExpLang Interpreter
//
// Usage: linux> scala ELInterp <source file>
//
//
////////////////////////////////////////////////////////////////////////////////
// Name: Camilo Schaser-Hughes
// Class: CS558, Prog Languages
// Date: Oct 15, 2022
// Worked in a study group with Nick Giampietro, Aung Baw and 
// Rhitabrat Pokharel where it was Nick's idea about the unwrapping helper
// functions. All the code is my own tho.
////////////////////////////////////////////////////////////////////////////////
import ExpLang._

object ELInterp {
  case class InterpException(string: String) extends RuntimeException

  // unwraping function, throws an error if not a left, 
  // returns the boolean unwrapped.
  def boolOrError(e: Either[Boolean, Int]): Boolean = e match {
    case Left(b) => b
    case _ => throw InterpException("We need a bool but we got: " + e)
  }

  // unwraping function, throws an error if not a right, 
  // returns the integer unwrapped.
  def intOrError(e: Either[Boolean, Int]): Int = e match {
    case Right(i) => i
    case _ => throw InterpException("We need an int but we got: " + e)
  }

  // returns true if bool, false if int.
  def boolOrFalse(e: Either[Boolean, Int]): Boolean = e match {
    case Left(b) => true
    case Right(i) => false
  }
  
  // these mostly just unwrap the objects, does the calculations
  // and then rewraps them and sends them on their way.
  def interp(e:Expr): Either[Boolean,Int] = e match {
    case True => Left(true)
    case False => Left(false)
    case Num(n) => Right(n)
    case Not(e)  => Left(! boolOrError(interp(e))) // done
    case And(l,r) => Left(boolOrError(interp(l)) && boolOrError(interp(r))) // done
    case Or(l,r)  => Left(boolOrError(interp(l)) || boolOrError(interp(r))) // done
    case Xor(l,r) => {
      val lft = boolOrError(interp(l))
      val rgt = boolOrError(interp(r))
      Left((lft && (!rgt)) || ((!lft) && rgt))
      } // done
    case Add(l,r) => Right(intOrError(interp(l)) + intOrError(interp(r))) // done
    case Sub(l,r) => Right(intOrError(interp(l)) - intOrError(interp(r))) // done
    case Mul(l,r) => Right(intOrError(interp(l)) * intOrError(interp(r))) // done
    // for div and rem just stores the unwrapped elements while they
    // check to see if r == 0.
    case Div(l,r) => {
      val lft = intOrError(interp(l))
      val rgt = intOrError(interp(r))
      if (rgt == 0) throw InterpException("Can't div by zero, it's infinite!")
      else Right(lft / rgt)
      } // done
    case Rem(l,r) => { 
      val lft = intOrError(interp(l))
      val rgt = intOrError(interp(r))
      if (rgt == 0) throw InterpException("Can't rem by zero, it's infinite!")
      else Right(lft % rgt)
      } // done
    case Lt(l,r) => Left(intOrError(interp(l)) < intOrError(interp(r))) // done
    case Gt(l,r) => Left(intOrError(interp(l)) > intOrError(interp(r))) // done
    // uses a true false helper function to check if it's bool or int
    // before using the unwrapper function and doing the actual == checking.
    case Eq(l,r) => {
      val lftw = interp(l)
      val rgtw = interp(r)
      if (boolOrFalse(lftw) && boolOrFalse(rgtw)) {
        return Left(boolOrError(lftw) == boolOrError(rgtw))
      } else if ((!boolOrFalse(lftw)) && (!boolOrFalse(rgtw))) {
        return Left(intOrError(lftw) == intOrError(rgtw))
      } else {
        throw InterpException("Can't compare Ints and Bools!")
      }
    } // done
    case If(c,l,r) => {
      if (boolOrError(interp(c))) Right(intOrError(interp(l)))
      else Right(intOrError(interp(r))) 
      } // done, i think.
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
