package mupl.oop

object Macros {

  def ifEq(e1: Expr, e2: Expr, e3: Expr, e4: Expr) =
    Let("_x", e1, Let("_y", e2, IfGreater(Var("_x"), Var("_y"), e4, IfGreater(Var("_y"), Var("_x"), e4, e3))))

  def ifUnit(e1: Expr, e2: Expr, e3: Expr) =
    IfGreater(IsUnit(e1), Num(0), e2, e3)

  def mlet(ls: List[(String, Expr)], e: Expr): Expr = ls match {
    case Nil => e
    case x :: xs => Let(x._1, x._2, mlet(xs, e))
  }

  def list(es: Expr*): Expr = es.toList match {
    case Nil => Unit
    case e :: es => Pair(e, list(es: _*))
  }
}
