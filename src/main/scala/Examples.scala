import extensions.UserExtensionBoolean._
import mupl.oop._
import Mupl._
import Macros._

object Examples extends App {

  val sqr = Fun(Some("sqr"), "x", Mult(Var("x"), Var("x")))
  val f = Fun(None, "a", Fun(None, "b", Add(Var("a"), Var("b"))))

  val call1 = Call(Call(f, Num(5)), Num(5))
  val call2 = Call(sqr, Num(16))

  val bool = And(Or(True, False), Or(True, False))
  val e = If(bool, call1, call2)

  println(e.eval)

  val fun = Fun(None, "pair", Add(Fst(Var("pair")), Snd(Var("pair"))))

  val curried = Call(curry, fun)
  println("Curried fun evaluation: " + Call(Call(curried, Num(5)), Num(100)).eval)

  val uncurried = Call(uncarry, curried)
  println("Uncurried fun evaluation: " + Call(uncurried, Pair(Num(5), Num(100))).eval)

  //map function implemented via curry function
  val m = Call(curry, Fun(Some("_fun_"), "pair", ifUnit(Snd(Var("pair")), Unit, Pair(
    Call(Fst(Var("pair")), Fst(Snd(Var("pair")))), Call(Var("_fun_"), Pair(Fst(Var("pair")), Snd(Snd(Var("pair")))))))))

  //list mapping with increment function
  val l1 = list(Num(10), Num(20), Num(30))
  val increment = Fun(None, "x", Add(Var("x"), Num(1)))
  val x = Call(Call(map, increment), l1)
  val y = Call(Call(m, increment), l1)

  println(x.eval)
  println(y.eval)


  val greaterThan10 = Fun(None, "x", IfGreater(Var("x"), Num(10), Num(1), Num(0)))

  println(Call(Call(filter, greaterThan10), l1).eval)


}
