package miniscala

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
 * Abstract syntax tree representation of MiniScala programs.
 */
object Ast {

  /**
   * An AST node contains information about its position in the source code.
   */
  sealed abstract class AstNode extends Positional

  /**
   * Identifiers are just strings.
   */
  type Var = String

  type Fun = String

  /**
   * Expressions (excluding literals).
   */
  sealed abstract class Exp extends AstNode

  case class VarExp(x: Var) extends Exp

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp

  case class IfThenElseExp(condexp: Exp, thenexp: Exp, elseexp: Exp) extends Exp

  case class BlockExp(vals: List[ValDecl], defs: List[DefDecl], exp: Exp) extends Exp

  case class TupleExp(exps: List[Exp]) extends Exp

  case class MatchExp(exp: Exp, cases: List[MatchCase]) extends Exp

  case class CallExp(fun: Fun, args: List[Exp]) extends Exp

  /**
   * Literals.
   */
  sealed abstract class Literal extends Exp

  case class IntLit(c: Int) extends Literal

  case class BoolLit(c: Boolean) extends Literal

  case class FloatLit(c: Float) extends Literal

  case class StringLit(c: String) extends Literal

  /**
   * Binary operators.
   */
  sealed abstract class BinOp extends AstNode

  case class PlusBinOp() extends BinOp

  case class MinusBinOp() extends BinOp

  case class MultBinOp() extends BinOp

  case class DivBinOp() extends BinOp

  case class EqualBinOp() extends BinOp

  case class LessThanBinOp() extends BinOp

  case class LessThanOrEqualBinOp() extends BinOp

  case class ModuloBinOp() extends BinOp

  case class MaxBinOp() extends BinOp

  case class AndBinOp() extends BinOp

  case class OrBinOp() extends BinOp

  /**
   * Unary operators.
   */
  sealed abstract class UnOp extends AstNode

  case class NegUnOp() extends UnOp

  case class NotUnOp() extends UnOp

  /**
   * Declarations.
   */
  sealed abstract class Decl extends AstNode

  case class ValDecl(x: Var, opttype: Option[Type], exp: Exp) extends Decl

  case class DefDecl(fun: Fun, params: List[FunParam], optrestype: Option[Type], body: Exp) extends Decl

  /**
    * Function parameters.
    */
  case class FunParam(x: Var, opttype: Option[Type]) extends AstNode

  /**
   * Match cases.
   */
  case class MatchCase(pattern: List[Var], exp: Exp) extends AstNode

  /**
   * Types.
   */
  sealed abstract class Type extends AstNode

  case class IntType() extends Type

  case class BoolType() extends Type

  case class FloatType() extends Type

  case class StringType() extends Type

  case class TupleType(types: List[Type]) extends Type

  /**
   * Exception with a message and (optionally) a source code position.
   */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
