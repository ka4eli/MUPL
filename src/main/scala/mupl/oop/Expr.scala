package mupl.oop

import mupl.exceptions.MuplEvalException

//MUPL - Made Up Programming Language

import Expr._

/**
 * Basic trait for all MUPL expression. Implement evaluator for new MUPL expression and call eval to evaluate it
 */
trait Expr {

  def evaluator: Evaluator

  /**
   * Evaluates this MUPL expression to MUPL value
   *
   * MUPL values (in default implementation) are: Num, Closure, Unit, or a MUPL Pair of MUPL values.
   * MUPL expressions are: Var, Add, Mult, IfGreater, Fun, Call, Let, Pair, Fst, Snd, IsUnit
   */
  def eval: Expr = computeFreeVars(Set())._1.evalUnderEnv(Nil)

  /**
   * When building a closure, it uses an environment that is like the current
   * environment but holds only variables that are free variables in the function part of the closure. (A free
   * variable is a variable that appears in the function without being under some shadowing binding for the
   * same variable.)
   */
  private def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = evaluator.computeFreeVars(fv)

  /**
   * Helper function which evaluates this MUPL expression under environment env to MUPL value
   */
  private def evalUnderEnv(env: List[(String, Expr)]): Expr = evaluator.evalUnderEnv(env)
}

case class Var(varName: String) extends Expr {
  self =>

  private def envLookup(env: List[(String, Expr)], name: String): Expr = {
    if (env.isEmpty) error("unbound variable during evaluation: " + name)
    else if (env.head._1 == name) env.head._2
    else envLookup(env.tail, name)
  }

  val evaluator = new Evaluator {
    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv + varName)

    def evalUnderEnv(env: List[(String, Expr)]): Expr = envLookup(env, varName)
  }
}

case class Num(i: Int) extends Expr {
  self =>
  val evaluator = new Evaluator {
    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv)

    def evalUnderEnv(env: List[(String, Expr)]): Expr = self
  }
}

case class Add(e1: Expr, e2: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e1.evaluator.computeFreeVars(fv)
      val (v2, s2) = e2.evaluator.computeFreeVars(fv)
      (Add(v1, v2), s1 ++ s2)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val v1 = e1.evaluator.evalUnderEnv(env)
      val v2 = e2.evaluator.evalUnderEnv(env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) => Num(i1 + i2)
        case _ => error("MUPL Add applied to non-number")
      }
    }
  }
}

case class Mult(e1: Expr, e2: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e1.evaluator.computeFreeVars(fv)
      val (v2, s2) = e2.evaluator.computeFreeVars(fv)
      (Mult(v1, v2), s1 ++ s2)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val v1 = e1.evaluator.evalUnderEnv(env)
      val v2 = e2.evaluator.evalUnderEnv(env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) => Num(i1 * i2)
        case _ => error("MUPL Mult applied to non-number")
      }
    }
  }
}

case class IfGreater(e1: Expr, e2: Expr, then: Expr, els: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {
    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e1.evaluator.computeFreeVars(fv)
      val (v2, s2) = e2.evaluator.computeFreeVars(fv)
      val (v3, s3) = then.evaluator.computeFreeVars(fv)
      val (v4, s4) = els.evaluator.computeFreeVars(fv)
      (IfGreater(v1, v2, v3, v4), s1 ++ s2 ++ s3 ++ s4)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val v1 = e1.evaluator.evalUnderEnv(env)
      val v2 = e2.evaluator.evalUnderEnv(env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) =>
          if (i1 > i2) then.evaluator.evalUnderEnv(env)
          else els.evaluator.evalUnderEnv(env)
        case _ => error("MUPL IfGreater applied to non-number")
      }
    }
  }
}

case class Fun(name: Option[String], param: String, body: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val a = body.evaluator.computeFreeVars(fv)
      val fvUpd = a._2.filterNot(name.contains(_)) - param
      (FunFV(name, param, a._1, fvUpd), fvUpd)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = self //or throw error
  }
}

case class Call(funExpr: Expr, arg: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = funExpr.evaluator.computeFreeVars(fv)
      val (v2, s2) = arg.evaluator.computeFreeVars(fv)
      (Call(v1, v2), s1 ++ s2)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val cl = funExpr.evaluator.evalUnderEnv(env)
      cl match {
        case Closure(en, fun) =>
          val argVal = arg.evaluator.evalUnderEnv(env)
          val c = fun.evaluator.evalUnderEnv(env)
          val (funName, argName) = (c.fun.name, c.fun.param)

          val newEnv = funName match {
            case Some(n) => (n, cl) ::(argName, argVal) :: en
            case None => (argName, argVal) :: en
          }

          fun.evaluator.evalUnderEnv(env).fun.body.evaluator.evalUnderEnv(newEnv)

        case _ => error("MUPL Call applied to non-function")
      }
    }
  }
}

case class Let(s: String, e: Expr, body: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e.evaluator.computeFreeVars(fv)
      val (v2, s2) = body.evaluator.computeFreeVars(fv)
      (Let(s, v1, v2), (s1 ++ s2) - s)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val v1 = e.evaluator.evalUnderEnv(env)
      body.evaluator.evalUnderEnv((s, v1) :: env)
    }
  }
}

case class Pair(e1: Expr, e2: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e1.evaluator.computeFreeVars(fv)
      val (v2, s2) = e2.evaluator.computeFreeVars(fv)
      (Pair(v1, v2), s1 ++ s2)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      val v1 = e1.evaluator.evalUnderEnv(env)
      val v2 = e2.evaluator.evalUnderEnv(env)
      Pair(v1, v2)
    }
  }
}

case class Fst(e: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e.evaluator.computeFreeVars(fv)
      (Fst(v1), s1)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = e.evaluator.evalUnderEnv(env) match {
      case Pair(e1, e2) => e1.evaluator.evalUnderEnv(env)
      case _ => error("MUPL Fst applied to non-pair")
    }
  }
}

case class Snd(e: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e.evaluator.computeFreeVars(fv)
      (Snd(v1), s1)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = e.evaluator.evalUnderEnv(env) match {
      case Pair(e1, e2) => e2.evaluator.evalUnderEnv(env)
      case _ => error("MUPL Snd applied to non-pair")
    }
  }
}

case object Unit extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (Unit, fv)

    def evalUnderEnv(env: List[(String, Expr)]): Expr = self
  }
}

case class IsUnit(e: Expr) extends Expr {
  self =>
  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
      val (v1, s1) = e.evaluator.computeFreeVars(fv)
      (IsUnit(v1), s1)
    }

    def evalUnderEnv(env: List[(String, Expr)]): Expr = {
      e.evaluator.evalUnderEnv(env) match {
        case Unit => Num(1)
        case _ => Num(0)
      }
    }
  }
}

case class FunFV private[oop](name: Option[String], param: String, body: Expr, freeVars: Set[String]) extends Expr {
  self =>
  private def getClosureEnv(fun: FunFV, env: List[(String, Expr)]) = {
    def loop(s: Set[String], env: List[(String, Expr)], acc: List[(String, Expr)]): List[(String, Expr)] = {
      if (s.isEmpty || env.isEmpty) acc
      else if (s.contains(env.head._1)) loop(s - env.head._1, env.tail, env.head :: acc)
      else loop(s, env.tail, acc)
    }
    loop(fun.freeVars, env, Nil)
  }

  val evaluator = new Evaluator {

    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv)

    def evalUnderEnv(env: List[(String, Expr)]): Closure = Closure(getClosureEnv(self, env), self)
  }
}

case class Closure private[oop](env: List[(String, Expr)], fun: FunFV) extends Expr {
  self =>
  val evaluator = new Evaluator {
    def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv)

    def evalUnderEnv(env: List[(String, Expr)]): Expr = self
  }
}

trait Evaluator {
  def computeFreeVars(fv: Set[String]): (Expr, Set[String])

  def evalUnderEnv(env: List[(String, Expr)]): Expr
}

object Expr {
  def error(mes: String) = throw new MuplEvalException(mes)
}
