package miniscala

import miniscala.Ast._
import miniscala.Interpreter.InterpreterError
import miniscala.Unparser.unparse

/**
 * Type checker for MiniScala.
 */
object TypeChecker {

  type VarTypeEnv = Map[Var, Type]

  type FunTypeEnv = Map[Fun, (List[Type], Type)]

  def typeCheck(e: Exp, vtenv: VarTypeEnv, ftenv: FunTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) =>
      vtenv.getOrElse(x, throw new InterpreterError(s"Unknown type '$x'", e))

    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, vtenv, ftenv)
      val righttype = typeCheck(rightexp, vtenv, ftenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), IntType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), FloatType()) => FloatType()
            case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case EqualBinOp() => BoolType()
        case LessThanBinOp() | LessThanOrEqualBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => BoolType()
            case (FloatType(), IntType()) => BoolType()
            case (IntType(), FloatType()) => BoolType()
            case (FloatType(), FloatType()) => BoolType()
            case (BoolType(), BoolType()) => BoolType()
            case (StringType(), StringType()) => BoolType()
            case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }

        case AndBinOp() | OrBinOp() => (lefttype,righttype) match{
          case (BoolType(),BoolType()) =>BoolType()
          case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
        }
      }
    case UnOpExp(op, exp) =>
      val typeexp = typeCheck(exp,vtenv,ftenv)
      op match{
      case NegUnOp() => typeexp match{
        case IntType() => IntType()
        case FloatType() => FloatType()
        case _ => throw new TypeError(s"Type mismatch, unexpected type ${unparse(typeexp)} ", op)
      }
      case NotUnOp() =>typeexp match{
        case BoolType() => BoolType()
        case _ => throw new TypeError(s"Type mismatch, unexpected type ${unparse(typeexp)} ", op)
      }
    }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val ct = typeCheck(condexp,vtenv,ftenv)
      val tt = typeCheck(thenexp, vtenv,ftenv)
      val et = typeCheck(elseexp, vtenv,ftenv)
      if (ct == BoolType() && tt == et) tt else {
         throw new TypeError(s"Type mismatch, unexpected types ${unparse(ct)} ,${unparse(tt)} and ${unparse(et)}", e)
      }

    case BlockExp(vals, defs, exp) =>
      var tenv = (vtenv, ftenv)
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv._1, tenv._2)
        checkTypesEqual(t, d.opttype, d)
        tenv = (tenv._1 + (d.x -> d.opttype.getOrElse(t)), tenv._2)
      }
      for (d <- defs){
        val tps = getFunType(d)
        tenv = (tenv._1, tenv._2 + (d.fun ->(tps._1,tps._2)))
      }
      for (d <- defs){
        var vtenv1 = tenv._1
        d.params.foreach(p => vtenv1 += (p.x -> p.opttype.get))
        val tt = typeCheck(d.body,vtenv1,tenv._2)
        checkTypesEqual(tt, d.optrestype,d)
      }
      typeCheck(exp,tenv._1,tenv._2)

    case TupleExp(exps) =>
      var l1 = List[Type]()
      for (e <- exps){
        l1 = typeCheck(e,vtenv,ftenv):: l1
      }
      TupleType(l1.reverse)
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, vtenv, ftenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              var vtenv1 = vtenv
              for(d <- c.pattern.zip(ts)){
                vtenv1 += d
              }
              return typeCheck(c.exp,vtenv1,ftenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(fun, args) =>
      val (t1,t2) = ftenv.getOrElse(fun, throw new TypeError(s"Error when retrieving types for function $fun",e))
      val argtps = args.map(typeCheck(_,vtenv,ftenv))
      if(t1 != argtps){
        throw new TypeError("Mismatch in expected argument types",e)
      }
      t2
  }

  /**
    * Returns the parameter types and return type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): (List[Type], Type) =
    (d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
   * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
   */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
   * Builds an initial type environment, with a type for each free variable in the program.
   */
  def makeInitialVarTypeEnv(program: Exp): VarTypeEnv = {
    var vtenv: VarTypeEnv = Map()
    for (x <- Vars.freeVars(program))
      vtenv = vtenv + (x -> IntType())
    vtenv
  }

  /**
   * Exception thrown in case of MiniScala type errors.
   */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
