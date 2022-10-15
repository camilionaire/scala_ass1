//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Author: J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ExpLang Compiler
//
import org.scalatest.FunSuite
import ELComp._
import StackM1._

class TestELC extends FunSuite {

  // normal cases
  test("correctly compile and expressions") {
    assertResult(1)(ELComp("(and t t)"))
    assertResult(0)(ELComp("(and f t)"))
    assertResult(0)(ELComp("(and t f)"))
    assertResult(0)(ELComp("(and f f)"))
  }

  test("correctly compiles not expressions") {
    assertResult(1)(ELComp("(not f)"))
    assertResult(0)(ELComp("(not t)"))
  }

  test("correctly compile boolean expressions") {
    assertResult(0)(ELComp("(not (and (or f t) t))"))
    assertResult(1)(ELComp("(and (xor t f) (xor f t))"))
    assertResult(1)(ELComp("(xor (and t f) (or f t))"))
    assertResult(1)(ELComp("(and 1 t)"))
  }
	
  // test("correctly compile integer expressions") {
  //   assertResult(2)(ELComp("(+ 1 (- 3 2))"))
  //   assertResult(11)(ELComp("(+ 1 (* 5 (- 4 2)))"))
  //   assertResult(9)(ELComp("(- (* 2 5) (% 7 3))"))
  //   assertResult(2)(ELComp("(+ 1 t)"))
  // } 
  
  // test("correctly compile relational expressions") {
  //   assertResult(1)(ELComp("(< 1 2)"))
  //   assertResult(0)(ELComp("(> 3 (+ 4 5))"))
  //   assertResult(1)(ELComp("(== (* 2 2) (/ 8 2))"))
  //   assertResult(0)(ELComp("(== t f)"))
  //   assertResult(0)(ELComp("(> 1 t)"))
  //   assertResult(0)(ELComp("(< 1 t)"))
  //   assertResult(1)(ELComp("(== 1 t)"))
  // } 
  
  // test("correctly compile if expressions") {
  //   assertResult(2)(ELComp("(if (not f) 2 3)"))
  //   assertResult(3)(ELComp("(if (> 4 5) 2 3)"))
  //   assertResult(1)(ELComp("(if t t t)"))
  //   assertResult(2)(ELComp("(if 1 2 3)"))
  //   assertResult(0)(ELComp("(if t f 4)"))
  // } 
  
  // // exception cases
  // test("divide-by-zero exceptions") {
  //   intercept[ExecException] { ELComp("(/ 1 0)") }
  //   intercept[ExecException] { ELComp("(% 1 (- 5 5))") }
  // }

}
