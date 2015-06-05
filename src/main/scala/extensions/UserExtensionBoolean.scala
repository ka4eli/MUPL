package extensions

import mupl.oop._

/**
 * Example of how user can extend MUPL with his own expressions
 */
object UserExtensionBoolean {

  import Expr._

  case object True extends Expr {
    self =>
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = self

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv)
    }
  }

  case object False extends Expr {
    self =>
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = self

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = (self, fv)
    }
  }

  case class Not(e: Expr) extends Expr {
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = e.evaluator.evalUnderEnv(env) match {
        case True => False
        case False => True
        case _ => error("MUPL Not applied to non-boolean MUPL: " + e)
      }

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
        val (v1, s1) = e.evaluator.computeFreeVars(fv)
        (Not(v1), s1)
      }
    }
  }

  case class And(e1: Expr, e2: Expr) extends Expr {
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = e1.evaluator.evalUnderEnv(env) match {
        case True => e2.evaluator.evalUnderEnv(env) match {
          case True => True
          case False => False
          case _ => error("MUPL And applied to non-boolean MUPL: " + e2)
        }
        case False => False
        case _ => error("MUPL And applied to non-boolean MUPL: " + e1)
      }

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
        val (v1, s1) = e1.evaluator.computeFreeVars(fv)
        val (v2, s2) = e2.evaluator.computeFreeVars(fv)
        (And(v1, v2), s1 ++ s2)
      }
    }
  }

  case class Or(e1: Expr, e2: Expr) extends Expr {
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = e1.evaluator.evalUnderEnv(env) match {
        case False => e2.evaluator.evalUnderEnv(env) match {
          case True => True
          case False => False
          case _ => error("MUPL Or applied to non-boolean MUPL: " + e2)
        }
        case True => True
        case _ => error("MUPL Or applied to non-boolean MUPL: " + e1)
      }

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
        val (v1, s1) = e1.evaluator.computeFreeVars(fv)
        val (v2, s2) = e2.evaluator.computeFreeVars(fv)
        (Or(v1, v2), s1 ++ s2)
      }
    }
  }

  case class If(cond: Expr, e1: Expr, e2: Expr) extends Expr {
    val evaluator = new Evaluator {
      def evalUnderEnv(env: List[(String, Expr)]): Expr = cond.evaluator.evalUnderEnv(env) match {
        case True => e1.evaluator.evalUnderEnv(env)
        case False => e2.evaluator.evalUnderEnv(env)
        case _ => error("MUPL If applied to non-boolean MUPL" + cond)
      }

      def computeFreeVars(fv: Set[String]): (Expr, Set[String]) = {
        val (v1, s1) = cond.evaluator.computeFreeVars(fv)
        val (v2, s2) = e1.evaluator.computeFreeVars(fv)
        val (v3, s3) = e2.evaluator.computeFreeVars(fv)
        (If(v1, v2, v3), s1 ++ s2 ++ s3)
      }
    }
  }

}
