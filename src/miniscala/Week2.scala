package miniscala

import miniscala.Ast.{BinOpExp, DivBinOp, Exp, IntLit, MaxBinOp, MiniScalaError, MinusBinOp, ModuloBinOp, MultBinOp, NegUnOp, PlusBinOp, UnOpExp}
import miniscala.Interpreter.{InterpreterError, trace}

object Week2 {

  type Var = String
  sealed abstract class VarEnv
  private case class ConsVarEnv(x: Var, v: Int, next: VarEnv) extends VarEnv
  private case object NilVarEnv extends VarEnv

  def makeEmpty(): VarEnv = NilVarEnv
  def extend(e: VarEnv, x: Var, v: Int): VarEnv = ConsVarEnv(x, v, e)
  def lookup(e: VarEnv, x: Var): Int = e match {
    case ConsVarEnv(y, w, next) => if (x == y) w else lookup(next, x)
    case NilVarEnv => throw new RuntimeException("not found")
  }

  sealed abstract class IntList

  case object Nil extends IntList {
    override def toString: String = ""
  }

  case class Cons(x: Int, xs: IntList) extends IntList {
    override def toString: String = {
      var s = s"$x, "
      xs match {
        case Nil => s = s.substring(0, s.length() - 2); s
        case Cons(x, ys) => s + s"$x, " + ys.toString
      }
    }
  }


  def append(xs: IntList, x: Int): IntList = xs match {
    case Nil => Cons(x, Nil)
    case Cons(y, ys) => Cons(y, append(ys, x))
  }

  def square(xs: IntList): IntList = xs match {
    case Nil => Nil
    case Cons(x, ys) => Cons(x * x, square(ys))
  }

  def length(xs: IntList): Int = xs match {
    case Nil => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def ordered(xs: IntList): Boolean = xs match {
    case Nil => true
    case Cons(x, ys) => ys match {
      case Nil => true
      case Cons(x1, zs) => if (x1 >= x) ordered(ys) else false
    }
  }

  def reverse(xs: IntList): IntList = xs match {
    case Nil => Nil
    case Cons(x, ys) => append(reverse(ys), x)
  }

  def odd(xs: IntList): IntList = {
    val acc: IntList = Nil
    xs match {
      case Nil => Nil
      case Cons(x, ys) => if (length(ys) % 2 == 0) append(acc, x); odd(ys)
    }
    acc
  }

  def countIntLits(e: Exp): Int = e match{
    case IntLit(_) => 1
    case BinOpExp(l, _, r) => countIntLits(l) + countIntLits((r))
    case UnOpExp(_,e) => countIntLits(e)
  }

  def varEnvToMap(env: VarEnv): Map[Var, Int] =
    env match{
      case NilVarEnv => Map.empty
      case ConsVarEnv(x,i,next) => varEnvToMap(next) + (x->i)
    }

  def mapToVarEnv(env: Map[Var,Int]): VarEnv ={
    var v1 = makeEmpty()
    for ((x,y) <- env){
      v1 = extend(v1,x,y)
    }
    v1
  }

  sealed abstract class Nat

  case object Zero extends Nat

  case class Succ(n: Nat) extends Nat

  def decode(n: Nat): Int = n match {
    case Zero => 0
    case Succ(n) => 1 + decode(n)
  }

  def encode(i: Int): Nat = {
    if(i < 0) throw new MiniScalaError("No negative inputs")
    if (i == 0) Zero else Succ(encode(i - 1))
  }

  def add(a: Nat, b: Nat): Nat = a match{
    case Zero => b
    case Succ(a) => add(a,Succ(b))
  }


  def mult(a: Nat, b: Nat): Nat = a match {
    case Zero => Zero
    case Succ(n) => add(b,mult(n,b))
  }

  def power(a: Nat, b: Nat): Nat = b match {
    case Zero => Succ(Zero)
    case Succ(n) =>mult(a,power(a,n))
  }

  def decrement(n: Nat): Nat = n match {
    case Zero => Zero
    case Succ(n) => n
  }

  def main(args: Array[String]): Unit = {

  }
}

