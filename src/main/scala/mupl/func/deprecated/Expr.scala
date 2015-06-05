package mupl.func.deprecated

/**
 * This is a functional version of MUPL, can't be extended. All classes are deprecated
 */
@deprecated
sealed trait Expr

case class Var(s: String) extends Expr
case class Num(i: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class IfGreater(e1: Expr, e2: Expr, then: Expr, els: Expr) extends Expr
case class Fun(name: Option[String], param: String, body: Expr) extends Expr
case class FunFV private[mupl](name: Option[String], param: String, body: Expr, freeVars: Set[String]) extends Expr
case class Call(funExpr: Expr, arg: Expr) extends Expr
case class Let(s: String, e: Expr, body: Expr) extends Expr
case class Pair(e1: Expr, e2: Expr) extends Expr
case class Fst(e: Expr) extends Expr
case class Snd(e: Expr) extends Expr
case object Unit extends Expr
case class IsUnit(e: Expr) extends Expr
case class Closure private[mupl](env: List[(String, Expr)], fun: FunFV) extends Expr
