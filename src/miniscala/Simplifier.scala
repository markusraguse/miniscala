package miniscala

import miniscala.Ast.{BinOpExp, BlockExp, DivBinOp, Exp, IntLit, MinusBinOp, ModuloBinOp, MultBinOp, NegUnOp, PlusBinOp, UnOpExp, VarExp}

object Simplifier {
  def simplify(exp: Exp): Exp = exp match {
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = simplify(leftexp)
      val rightval = simplify(rightexp)
      op match {
        case PlusBinOp() =>
          if (leftval == IntLit(0)) rightval
          else if (rightval == IntLit(0)) leftval
          else BinOpExp(leftval, op, rightval)
        case MinusBinOp() =>
          if (leftval == rightval) IntLit(0)
          else if (rightval == IntLit(0)) leftval
          else if (leftval == IntLit(0)) UnOpExp(NegUnOp(), rightval)
          else BinOpExp(leftval, op, rightval)
        case MultBinOp() =>
          if (leftval == IntLit(1)) rightval
          else if (rightval == IntLit(1)) leftval
          else if (leftval == IntLit(0)) IntLit(0)
          else if (rightval == IntLit(0)) IntLit(0)
          else BinOpExp(leftval, op, rightval)
        case DivBinOp() =>
          if (leftval == rightval) IntLit(1)
          else if (rightval == IntLit(1)) leftval
          else BinOpExp(leftval, op, rightval)
        case ModuloBinOp() =>
          if (leftval == rightval) IntLit(0)
          else BinOpExp(leftval, op, rightval)
      }
    case UnOpExp(op, exp) => UnOpExp(op, exp)
    case IntLit(c) => IntLit(c)
    case VarExp(x) => VarExp(x)
    //case BlockExp(vals, e) => BlockExp(vals, simplify(e))
  }
}
