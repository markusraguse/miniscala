package miniscala

import miniscala.Ast._
import miniscala.Interpreter.eval

/**
 * Unparser for MiniScala.
 */
object Unparser {

  def unparse(n: AstNode): String =
    n match {
      case IntLit(n) => "" + n
      case BoolLit(c) => "" + c
      case FloatLit(c) => s"${c}f"
      case StringLit(c) => "\"" + c +  "\""
      case IntType() => "Int"
      case FloatType() => "Float"
      case BoolType() => "Boolean"
      case StringType() => "String"
      case TupleType(types) => types.map(unparse).mkString("(",", ",")")
      case ValDecl(x,t,exp) => t match {
        case None => s"$x = ${unparse(exp)};"
        case Some(t) => s"$x: ${unparse(t)} = ${unparse(exp)}"
      }
      case VarExp(x) => x
      case BinOpExp(leftexp, op, rightexp) =>
        val leftval = unparse(leftexp)
        val rightval = unparse(rightexp)
        op match {
          case PlusBinOp() => s"($leftval + $rightval)"
          case MinusBinOp() => s"($leftval - $rightval)"
          case MultBinOp() => s"($leftval * $rightval)"
          case DivBinOp() => s"($leftval / $rightval)"
          case ModuloBinOp() => s"($leftval % $rightval)"
          case MaxBinOp() => s"($leftval max $rightval)"
          case EqualBinOp() => s"($leftval == $rightval)"
          case LessThanBinOp() => s"($leftval < $rightval)"
          case LessThanOrEqualBinOp() => s"($leftval <= $rightval)"
          case AndBinOp() =>s"($leftval && $rightval)"
          case OrBinOp() => s"($leftval || $rightval)"
        }
      case UnOpExp(op, exp) =>
        val expval = unparse(exp)
        op match {
          case NegUnOp() => s"-$expval"
          case NotUnOp() => s"!$expval"
        }
      case IfThenElseExp(ce,te,ee) => s"if (${unparse(ce)}) ${unparse(te)} else ${unparse(ee)}"

      //case BlockExp(vals,defs exp) =>
        //var a1 = ""
        //for(d <- vals)
          //a1 = a1 + unparse(d) + "; "
        //s"{$a1${unparse(exp)}}"

      case TupleExp(exps) =>
        var a1 = "("
        for(e <- exps){
          a1 = a1 + s"${unparse(e)}, "
        }
        a1 = s"${a1.substring(0,a1.length() - 2)})"
        a1
      case MatchCase(pattern, exp) =>
        var a1 = s"case ("
        for (p <- pattern){
          a1 = a1 + s"$p, "
        }
        a1 = s"${a1.substring(0,a1.length() - 2)}) => ${unparse(exp)}; "
        a1

      case MatchExp(exp, cases) =>
        var a1 = s"${unparse(exp)} match { "
        for (c <- cases){
          a1 = a1 + unparse(c)
        }
        a1 = a1.substring(0,a1.length()- 2) + " }"
        a1
    }
}
