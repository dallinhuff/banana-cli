package com.dallinhuff.banana

import scala.annotation.tailrec

object Types:
  enum SurfaceExpression:
    case Num(n: Double)
    case Bool(b: Boolean)
    case Ident(i: String)
    case Bin(op: SurfaceOp, lhs: SurfaceExpression, rhs: SurfaceExpression)
    case Not(e: SurfaceExpression)
    case If(tst: SurfaceExpression, thn: SurfaceExpression, els: SurfaceExpression)
    case Let(i: String, bnd: SurfaceExpression, bdy: SurfaceExpression)
    case App(fn: SurfaceExpression, arg: SurfaceExpression)
    case Fun(param: String, bdy: SurfaceExpression)
    case Rec(name: String, param: String, bdy: SurfaceExpression)
    case Empty
    case Cons(head: SurfaceExpression, tail: SurfaceExpression)
    case ListCase(ls: SurfaceExpression, emp: SurfaceExpression, h: String, t: String, cns: SurfaceExpression)

  enum SurfaceOp:
    case +, -, *, /, ===, <>, <, >, <=, >=, &, |

  enum CoreExpression:
    case Num(n: Double)
    case Bool(b: Boolean)
    case Ident(i: String)
    case Bin(op: CoreOp, lhs: CoreExpression, rhs: CoreExpression)
    case Not(e: CoreExpression)
    case If(tst: CoreExpression, thn: CoreExpression, els: CoreExpression)
    case App(fn: CoreExpression, arg: CoreExpression)
    case Fun(param: String, bdy: CoreExpression)
    case Rec(name: String, param: String, bdy: CoreExpression)
    case Empty
    case Cons(head: CoreExpression, tail: CoreExpression)
    case ListCase(ls: CoreExpression, emp: CoreExpression, h: String, t: String, cns: CoreExpression)

  enum CoreOp:
    case +, *, /, ===, <, |

  enum Val:
    case Num(n: Double)
    case Bool(b: Boolean)
    case Fun(env: Map[String, Val], param: String, bdy: CoreExpression)
    case Rec(env: Map[String, Val], name: String, param: String, bdy: CoreExpression)
    case Empty
    case Cons(head: Val, tail: Val)

  extension (v: Val)
    def display: String =
      v match
        case Val.Num(n) => n.toString
        case Val.Bool(b) => b.toString
        case Val.Fun(env, param, bdy) => s"($param) => {...}"
        case Val.Rec(env, name, param, bdy) => s"$name: ($param) => {...}"
        case Val.Empty => "[]"
        case Val.Cons(head, tail) => listDisplay(s"[${head.display}", tail)

  @tailrec
  private def listDisplay(acc: String, curr: Val): String =
    curr match
      case Val.Empty => acc :+ ']'
      case Val.Cons(head, tail) => listDisplay(s"$acc, ${head.display}", tail)

