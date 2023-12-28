package com.dallinhuff.banana

import Types.{SurfaceExpression as Sx, SurfaceOp as So, CoreExpression}
import Types.CoreExpression.*
import Types.CoreOp.*

object Desugarer:
  def desugar(e: Sx): CoreExpression =
    e match
      case Sx.Num(n) => Num(n)
      case Sx.Bool(b) => Bool(b)
      case Sx.Ident(i) => Ident(i)
      case Sx.Not(e) => Not(desugar(e))
      case Sx.If(tst, thn, els) => If(desugar(tst), desugar(thn), desugar(els))
      case Sx.Let(i, bnd, bdy) => App(Fun(i, desugar(bdy)), desugar(bnd))
      case Sx.App(fn, arg) => App(desugar(fn), desugar(arg))
      case Sx.Fun(param, bdy) => Fun(param, desugar(bdy))
      case Sx.Rec(name, param, bdy) => Rec(name, param, desugar(bdy))
      case Sx.Empty => Empty
      case Sx.Cons(head, tail) => Cons(desugar(head), desugar(tail))
      case Sx.ListCase(ls, emp, h, t, cns) => ListCase(desugar(ls), desugar(emp), h, t, desugar(cns))
      case Sx.Bin(op, lhs, rhs) =>
        val (lh, rh) = (desugar(lhs), desugar(rhs))
        op match
          case So.+ => Bin(+, lh, rh)
          case So.- => Bin(+, lh, Bin(*, rh, Num(-1)))
          case So.* => Bin(*, lh, rh)
          case So./ => Bin(/, lh, rh)
          case So.=== => Bin(===, lh, rh)
          case So.<> => Not(Bin(===, lh, rh))
          case So.< => Bin(<, lh, rh)
          case So.> => Not(Bin(|, Bin(===, lh, rh), Bin(<, lh, rh)))
          case So.<= => Bin(|, Bin(===, lh, rh), Bin(<, lh, rh))
          case So.>= => Not(Bin(<, lh, rh))
          case So.& => Not(Bin(|, Not(lh), Not(rh)))
          case So.| => Bin(|, lh, rh)
end Desugarer

