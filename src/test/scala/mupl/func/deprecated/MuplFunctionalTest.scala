package mupl.func.deprecated

import mupl.func.deprecated.MuplFunctional._
import org.scalatest._

class MuplFunctionalTest extends FlatSpec with Matchers {

  "Add" should "return Num(-1)" in {
    val e = Add(Num(2), Num(-3))
    evalExpr(e) should be (Num(-1))
  }

  "Mult" should "return Num(6)" in {
    val e = Mult(Num(2), Num(3))
    evalExpr(e) should be (Num(6))
  }

  "IfGreater" should "return Num(4)" in {
    val e = IfGreater(Num(1), Num(2), Num(3), Num(4))
    evalExpr(e) should be(Num(4))
  }

  "Let" should "assign x to Num(2)" in {
    val e = Let("x", Num(2), Add(Num(3), Var("x")))
    evalExpr(e) should be(Num(5))
  }

  "Call" should "return Num(20)" in {
    val e = Call(Fun(None, "x", Mult(Var("x"), Num(10))), Num(2))
    evalExpr(e) should be(Num(20))
  }

  "Fst" should "return Num(2)" in {
    val e = Fst(Pair(Num(2), Num(3)))
    evalExpr(e) should be(Num(2))
  }

  "Snd" should "return Num(3)" in {
    val e = Snd(Pair(Num(2), Num(3)))
    evalExpr(e) should be(Num(3))
  }

  "IsUnit" should "return Num(0)" in {
    val e = IsUnit(Call(Fun(None, "x", Mult(Var("x"), Num(10))), Num(2)))
    evalExpr(e) should be(Num(0))
  }

  "IsUnit" should "return Num(1)" in {
    val e = IsUnit(Unit)
    evalExpr(e) should be(Num(1))
  }



}
