package miniscala

import miniscala.Interpreter._
import miniscala.Ast._
import miniscala.Unparser._
import miniscala.parser.Parser.parse
import miniscala.Simplifier.simplify
import miniscala.TypeChecker.{FunTypeEnv, VarTypeEnv, typeCheck}

object Test {
  def main(args: Array[String]): Unit = {
    //val exp1 = BinOpExp(IntLit(2), PlusBinOp(), BinOpExp(IntLit(2), MinusBinOp(), IntLit(1)))
    //assert(parse(unparse(parse("5 + 3 * 2"))) == parse("5 + 3 * 2"))
    //print(unparse(simplify(parse("{val x = 5 + 0; x }"))))
    var varmap = Map[Var, Val]()
    var funmap = Map[Fun, Closure]()
    var vtenv: VarTypeEnv = Map()
    var ftenv: FunTypeEnv = Map()
    //println(unparse(parse("{val x: (Int, Int) = (5, 3); val y: Int = 3; x + y}")))
    //println(eval(parse("(1, 2) match { case (a, b) => (a < b, a + b) }"),varmap))
    //print(unparse(parse("(2,true)")))
    //print(unparse(parse("(1, 2) match { case (a, b) => (a < b, a + b); case (a, b, c) => (a + b + c) }")))
    //print(unparse(parse("{val x: Int = 5; x + 3}")))
    //print(parse(unparse(parse("\"Hej\""))))
    //println(eval(parse("{ def adder(n) = if (n <= 1) n else adder(n - 1); adder(3)}"),varmap,funmap))
    //println(eval(parse("{ def plusNum(n) = plusNum2(); { def plusNum2() = 5; plusNum2() }}"),varmap,funmap))
    //println(eval(parse( "{def adder(n: Int): Int = if (n <= 1) n else adder(n - 1); adder(3)}"),varmap,funmap))
    val a = parse("{ def f(x: Int): Int = x; f(2, 3) }")
    //println(typeCheck(a, vtenv,ftenv ))
    println(eval(parse("{ def f(x: Boolean): Int = x; f(true) }"),varmap,funmap))
  }
}
