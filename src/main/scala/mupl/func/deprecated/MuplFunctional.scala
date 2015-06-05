package mupl.func.deprecated

/**
 * MUPL - Made Up Programming Language
 */
@deprecated
object MuplFunctional {

  /**
   * Evaluates expression e written in MUPL to MUPL value
   *
   * MUPL values are: Num, Closure, Unit, or a MUPL Pair of MUPL values.
   * MUPL expressions are: Var, Add, Mult, IfGreater, Fun, Call, Let, Pair, Fst, Snd, IsUnit
   */
  @deprecated
  def evalExpr(e: Expr): Expr = evalUnderEnv(computeFreeVars(e), Nil)

  /**
   * When building a closure, it uses an environment that is like the current
   * environment but holds only variables that are free variables in the function part of the closure. (A free
   * variable is a variable that appears in the function without being under some shadowing binding for the
   * same variable.)
   */
  private def computeFreeVars(e: Expr): Expr = {
    def loop(e: Expr, fv: Set[String]): (Expr, Set[String]) = e match {
      case Var(s) => (e, fv + s)

      case Num(i) => (e, fv)

      case Add(e1, e2) =>
        val v1 = loop(e1, fv)
        val v2 = loop(e2, fv)
        (Add(v1._1, v2._1), v1._2 ++ v1._2)

      case Mult(e1, e2) =>
        val v1 = loop(e1, fv)
        val v2 = loop(e2, fv)
        (Mult(v1._1, v2._1), v1._2 ++ v1._2)

      case IfGreater(e1, e2, e3, e4) =>
        val (v1, s1) = loop(e1, fv)
        val (v2, s2) = loop(e2, fv)
        val (v3, s3) = loop(e3, fv)
        val (v4, s4) = loop(e4, fv)
        (IfGreater(v1, v2, v3, v4), s1 ++ s2 ++ s3 ++ s4)

      case Fun(n, p, b) =>
        val a = loop(b, fv)
        val fvUpd = a._2.filterNot(n.contains(_)) - p
        (FunFV(n, p, a._1, fvUpd), fvUpd)

      case Call(funExpr, arg) =>
        val (v1, s1) = loop(funExpr, fv)
        val (v2, s2) = loop(arg, fv)
        (Call(v1, v2), s1 ++ s2)

      case Let(s, ex, b) =>
        val (v1, s1) = loop(ex, fv)
        val (v2, s2) = loop(b, fv)
        (Let(s, v1, v2), (s1 ++ s2) - s)

      case Pair(e1, e2) =>
        val (v1, s1) = loop(e1, fv)
        val (v2, s2) = loop(e2, fv)
        (Pair(v1, v2), s1 ++ s2)

      case Fst(e1) =>
        val (v1, s1) = loop(e1, fv)
        (Fst(v1), s1)

      case Snd(e1) =>
        val (v1, s1) = loop(e1, fv)
        (Snd(v1), s1)

      case Unit => (Unit, fv)

      case IsUnit(e1) =>
        val (v1, s1) = loop(e1, fv)
        (IsUnit(v1), s1)

      case _ => throw new Exception("bad MUPL expression in compute-free-vars: " + e)
    }
    loop(e, Set())._1
  }

  /**
   * Lookup a variable in an environment
   */
  private def envLookup(env: List[(String, Expr)], name: String): Expr = {
    if (env.isEmpty) throw new Exception("unbound variable during evaluation: " + name)
    else if (env.head._1 == name) env.head._2
    else envLookup(env.tail, name)
  }

  /**
   * Returns effective closure environment (intersection between function free vars and global environment)
   */
  private def getClosureEnv(fun2: FunFV, env: List[(String, Expr)]) = {
    def loop(s: Set[String], env: List[(String, Expr)], acc: List[(String, Expr)]): List[(String, Expr)] = {
      if (s.isEmpty || env.isEmpty) acc
      else if (s.contains(env.head._1)) loop(s - env.head._1, env.tail, env.head :: acc)
      else loop(s, env.tail, acc)
    }
    loop(fun2.freeVars, env, Nil)
  }

  //Just for convenience
  private def evalUnderEnv(f: FunFV, env: List[(String, Expr)]): Closure = Closure(getClosureEnv(f, env), f)

  /**
   * Evaluates MUPL expression e under environment env to MUPL value
   */
  private def evalUnderEnv(e: Expr, env: List[(String, Expr)]): Expr = e match {
    case Var(s) => envLookup(env, s)

    case Add(e1, e2) =>
      val v1 = evalUnderEnv(e1, env)
      val v2 = evalUnderEnv(e2, env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) => Num(i1 + i2)
        case _ => throw new Exception("MUPL Add applied to non-number")
      }

    case Mult(e1, e2) =>
      val v1 = evalUnderEnv(e1, env)
      val v2 = evalUnderEnv(e2, env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) => Num(i1 * i2)
        case _ => throw new Exception("MUPL Mult applied to non-number")
      }

    case Num(i) => e

    case f: FunFV => Closure(getClosureEnv(f, env), f)

    case _: Closure => e

    case IfGreater(e1, e2, e3, e4) =>
      val v1 = evalUnderEnv(e1, env)
      val v2 = evalUnderEnv(e2, env)
      (v1, v2) match {
        case (Num(i1), Num(i2)) =>
          if (i1 > i2) evalUnderEnv(e3, env)
          else evalUnderEnv(e4, env)
        case _ => throw new Exception("MUPL IfGreater applied to non-number")
      }

    case Let(s, e, body) =>
      val v1 = evalUnderEnv(e, env)
      evalUnderEnv(body, (s, v1) :: env)

    case Call(func, arg) =>
      val cl = evalUnderEnv(func, env)
      cl match {
        case Closure(en, fun) =>
          val argVal = evalUnderEnv(arg, env)

          val c = evalUnderEnv(fun, env)
          val (funName, argName) = (c.fun.name, c.fun.param)

          val newEnv = funName match {
            case Some(n) => (n, cl) ::(argName, argVal) :: en
            case None => (argName, argVal) :: en
          }
          evalUnderEnv(evalUnderEnv(fun, env).fun.body, newEnv)

        case _ => throw new Exception("MUPL Call applied to non-function")
      }

    case Pair(e1, e2) =>
      val v1 = evalUnderEnv(e1, env)
      val v2 = evalUnderEnv(e2, env)
      Pair(v1, v2)

    case Fst(e) => evalUnderEnv(e, env) match {
      case Pair(e1, e2) => evalUnderEnv(e1, env)
      case _ => throw new Exception("MUPL Fst applied to non-pair")
    }

    case Snd(e) => evalUnderEnv(e, env) match {
      case Pair(e1, e2) => evalUnderEnv(e2, env)
      case _ => throw new Exception("MUPL Snd applied to non-pair")
    }

    case IsUnit(e) =>
      evalUnderEnv(e, env) match {
        case Unit => Num(1)
        case _ => Num(0)
      }
    case Unit => e

    case _ => throw new Exception("bad MUPL expression: " + e)
  }
}
