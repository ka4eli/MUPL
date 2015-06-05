package mupl.oop

object Mupl {

  import Macros._

  val curry = Fun(None, "fun", Fun(None, "x", Fun(None, "y", Call(Var("fun"), Pair(Var("x"), Var("y"))))))

  val uncarry = Fun(None, "fun", Fun(None, "pair", Call(Call(Var("fun"), Fst(Var("pair"))), Snd(Var("pair")))))

  val map = Fun(None, "f", Fun(Some("map"), "xs", ifUnit(Var("xs"), Unit, Pair(
    Call(Var("f"), Fst(Var("xs"))), Call(Var("map"), Snd(Var("xs")))))))

  val filter = Fun(None, "pred", Fun(Some("filter"), "xs", ifUnit(Var("xs"), Unit,
    IfGreater(Call(Var("pred"), Fst(Var("xs"))), Num(0), Pair(Fst(Var("xs")), Call(Var("filter"), Snd(Var("xs")))),
      Call(Var("filter"), Snd(Var("xs")))))))

}
