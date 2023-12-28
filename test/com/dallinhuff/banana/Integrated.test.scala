package com.dallinhuff.banana

import com.dallinhuff.banana.Parser
import Types.Val
import Val.*

def pdi(s: String): Either[String, Val] =
  for
    parsed <- Parser.parse(s)
    desugared <- Right(Desugarer.desugar(parsed))
    interpreted <- Interpreter.interpret(desugared)
  yield interpreted

class IntegratedTests extends munit.FunSuite:
  def assertSuccess(s: String, expected: Val) =
    assertEquals(pdi(s), Right(expected))

  def assertError(s: String) =
    assert(pdi(s).isLeft)

  test("number"):
    assertSuccess("4.2", Num(4.2))
    assertSuccess("4", Num(4.0))

  test("bool"):
    assertSuccess("#t", Bool(true))
    assertSuccess("#f", Bool(false))
    assertError("#tt")

  test("arithmetic bin"):
    assertSuccess("(+ 4 4)", Num(8.0))
    assertSuccess("(- 4 1)", Num(3))
    assertSuccess("(* 4 3)", Num(12))
    assertSuccess("(/ 8 4)", Num(2))

  test("compare num bin"):
    assertSuccess("(= 2 3)", Bool(false))
    assertSuccess("(= 2 2)", Bool(true))
    assertSuccess("(<> 2 3)", Bool(true))
    assertSuccess("(<> 2 2)", Bool(false))
    assertSuccess("(< 3 4)", Bool(true))
    assertSuccess("(< 4 4)", Bool(false))
    assertSuccess("(> 4 3)", Bool(true))
    assertSuccess("(> 4 4)", Bool(false))
    assertSuccess("(<= 4 4)", Bool(true))
    assertSuccess("(<= 5 4)", Bool(false))
    assertSuccess("(>= 5 4)", Bool(true))

  test("compare bool bin"):
    assertSuccess("(and #t #t)", Bool(true))
    assertSuccess("(and #t #f)", Bool(false))
    assertSuccess("(and #f #t)", Bool(false))
    assertSuccess("(and #f #f)", Bool(false))

    assertSuccess("(or #t #t)", Bool(true))
    assertSuccess("(or #t #f)", Bool(true))
    assertSuccess("(or #f #t)", Bool(true))
    assertSuccess("(or #f #f)", Bool(false))

  test("empty"):
    assertSuccess("(empty)", Empty)
    assertError("empty")
  
  test("cons"):
    assertSuccess("(cons #t (empty))", Cons(Bool(true), Empty))
    assertError("(cons 4 2)")

  test("if"):
    assertSuccess("(if #t 1 0)", Num(1.0))
    assertSuccess("(if #f 1 0)", Num(0.0))

  test("not"):
    assertSuccess("(not #t)", Bool(false))
    assertError("(not 9)")

  test("nested control flow"):
    assertSuccess("(if (and #t (= 2 (+ 1 1))) 9 8)", Num(9))

  test("let"):
    assertSuccess("(let ([x 4]) (* x 25))", Num(100))

  test("fun"):
    import Types.CoreExpression as CExpr
    import Types.CoreOp as COp
    assertSuccess("(fun (r) (* r 2))", Fun(Map.empty, "r", CExpr.Bin(COp.*, CExpr.Ident("r"), CExpr.Num(2))))

  test("app"):
    assertSuccess("(app (fun (x) (+ x 2)) 9)", Num(11))

  test("lexical scope"):
    val str = "(let ([x 2]) (let ([y 3]) (let ([x 4]) (+ x y))))"
    assertSuccess(str, Num(7))

  test("rec"):
    import Types.CoreExpression as CExpr
    import Types.CoreOp as COp

    val str = "(rec foo (bar) (= bar 9))"
    assertSuccess(str, Rec(Map.empty, "foo", "bar", CExpr.Bin(COp.===, CExpr.Ident("bar"), CExpr.Num(9))))

  test("app rec"):
    val str = "(app (rec cdown (n) (if (<= n 0) #t (app cdown (- n 1)))) 10)"
    assertSuccess(str, Bool(true))
