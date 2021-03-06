package miniscala

import miniscala.Ast._
import miniscala.TypeChecker.checkTypesEqual
import miniscala.Unparser.unparse

import scala.io.StdIn

/**
 * Interpreter for MiniScala.
 */
object Interpreter {

  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class BoolVal(v: Boolean) extends Val
  case class FloatVal(v: Float) extends Val
  case class StringVal(v: String) extends Val
  case class TupleVal(vs: List[Val]) extends Val

  case class Closure(params: List[FunParam], optrestype: Option[Type], body: Exp, venv: VarEnv, fenv: FunEnv,defs: List[DefDecl])

  type VarEnv = Map[Var, Val]
  type FunEnv = Map[Fun, Closure]

  def eval(e: Exp, venv: VarEnv, fenv: FunEnv): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => BoolVal(c)
    case FloatLit(c) => FloatVal(c)
    case StringLit(c) => StringVal(c)
    case VarExp(x) =>
      venv.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, venv, fenv)
      val rightval = eval(rightexp, venv, fenv)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 + v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 + v2)
            case (StringVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), IntVal(v2)) => StringVal(v1 + v2.toString)
            case (StringVal(v1), FloatVal(v2)) => StringVal(v1 + v2.toString)
            case (IntVal(v1), StringVal(v2)) => StringVal(v1.toString + v2)
            case (FloatVal(v1), StringVal(v2)) => StringVal(v1.toString + v2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 - v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 - v2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 * v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 * v2)
            case (StringVal(v1), IntVal(v2)) => StringVal(v1 * v2)
            //case  (IntVal(v1), StringVal(v2)) => StringVal(v2 * v1)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 / v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 / v2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 % v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 % v2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 == v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 == v2) BoolVal(true) else BoolVal(false)
            case (IntVal(v1), FloatVal(v2)) => if (v1 == v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), IntVal(v2)) => if (v1 == v2) BoolVal(true) else BoolVal(false)
            case (StringVal(v1), StringVal(v2)) => if (v1 == v2) BoolVal(true) else BoolVal(false)
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 == v2)
            case _ => BoolVal(false)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 < v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 < v2) BoolVal(true) else BoolVal(false)
            case (IntVal(v1), FloatVal(v2)) => if (v1 < v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), IntVal(v2)) => if (v1 < v2) BoolVal(true) else BoolVal(false)
            case (StringVal(v1), StringVal(v2)) => if (v1 < v2) BoolVal(true) else BoolVal(false)
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 < v2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)

          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 <= v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 <= v2) BoolVal(true) else BoolVal(false)
            case (IntVal(v1), FloatVal(v2)) => if (v1 <= v2) BoolVal(true) else BoolVal(false)
            case (FloatVal(v1), IntVal(v2)) => if (v1 <= v2) BoolVal(true) else BoolVal(false)
            case (StringVal(v1), StringVal(v2)) => if (v1 <= v2) BoolVal(true) else BoolVal(false)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 > v2) IntVal(v1) else IntVal(v2)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 > v2) FloatVal(v1) else FloatVal(v2)
            case (IntVal(v1), FloatVal(v2)) => if (v1 > v2) IntVal(v1) else FloatVal(v2)
            case (FloatVal(v1), IntVal(v2)) => if (v1 > v2) FloatVal(v1) else IntVal(v2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => if (v1 && v2) BoolVal(true) else BoolVal(false)
          case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
        case OrBinOp() => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => if (v1 || v2) BoolVal(true) else BoolVal(false)
          case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp, venv, fenv)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => IntVal(-v)
            case FloatVal(v) => FloatVal(-v)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() => expval match {
          case BoolVal(v) => BoolVal(!v)
          case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
        }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      if (eval(condexp, venv, fenv) == BoolVal(true)) eval(thenexp, venv, fenv) else eval(elseexp, venv, fenv)

    case BlockExp(vals, defs, exp) =>
      var venv1 = venv
      for (d <- vals) {
        val v = eval(d.exp, venv1, fenv)
        checkValueType(v, d.opttype, d)
        venv1 = venv1 + (d.x -> v)
      }
      var fenv1 = fenv
      for (d <- defs) {
        fenv1 = fenv1 + (d.fun -> Closure(d.params, d.optrestype, d.body, venv1, fenv1, defs))
      }

      eval(exp, venv1, fenv1)

    case TupleExp(exps) =>
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, venv, fenv) :: vals
      TupleVal(vals.reverse)
    case MatchExp(exp, cases) =>
      val expval = eval(exp, venv, fenv)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              var venv1 = venv
              for (d <- c.pattern.zip(vs)) {
                venv1 += d
              }
              return eval(c.exp, venv1, fenv)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(fun, args) =>
      val Closure(params, _, e, cvenv, cfenv, defs) = fenv(fun)
      var nvenv = cvenv
      var nfenv = cfenv

      if (args.length == params.length) {
        for ((a, p) <- args.zip(params)) {
          val av = eval(a, venv, fenv)
          checkValueType(av, p.opttype, e)
          nvenv = nvenv + (p.x -> av)
        }
      }
      else throw new InterpreterError("Type mismatch between ",e)

      for (d <- defs)
        nfenv = nfenv + (d.fun -> Closure(d.params, d.optrestype, d.body, cvenv, nfenv, defs))

      eval(e, nvenv, nfenv)
  }
  /**
   * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
   */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
   * Converts a value to its string representation (for error messages).
   */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(valueToString).mkString("(", ",", ")")
  }

  /**
   * Builds an initial environment, with a value for each free variable in the program.
   */
  def makeInitialVarEnv(program: Exp): VarEnv = {
    var venv = Map[Var, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      venv = venv + (x -> IntVal(StdIn.readInt()))
    }
    venv
  }

  /**
   * Prints message if option -trace is used.
   */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
   * Exception thrown in case of MiniScala runtime errors.
   */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
