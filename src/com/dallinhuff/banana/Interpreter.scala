package com.dallinhuff.banana

import Types.{CoreExpression as Cx, CoreOp, Val}
import CoreOp.*
import Val.*

object Interpreter:
  def interpret(expr: Cx, env: Map[String, Val] = Map.empty): Either[String, Val] =
    expr match
      case Cx.Num(n) => Right(Num(n))
      case Cx.Bool(b) => Right(Bool(b))
      case Cx.Ident(i) =>
        env.get(i).fold(Left(s"unbound identifier ($i)"))(Right.apply)
      case Cx.Bin(op, lhs, rhs) =>
        val (lh, rh) = (interpret(lhs, env), interpret(rhs, env))
        op match
          case + => (lh, rh) match
            case (Right(Num(l)), Right(Num(r))) => Right(Num(l + r))
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
            case _ => Left("addition terms must be numbers")
          case * => (lh, rh) match
            case (Right(Num(l)), Right(Num(r))) => Right(Num(l * r))
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
            case _ => Left("multiplication terms must be numbers")
          case / => (lh, rh) match
            case (Right(Num(l)), Right(Num(0))) =>
              Left("banana cannot divide by zero")
            case (Right(Num(l)), Right(Num(r))) =>
              Right(Num(l / r))
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
            case _ => Left("division terms must be numbers")
          case === => (lh, rh) match
            case (Right(Num(l)), Right(Num(r))) => Right(Bool(l == r))
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
            case _ => Left("equality terms must be numbers")
          case < =>
            (lh, rh) match
              case (Right(Num(l)), Right(Num(r))) => Right(Bool(l < r))
              case (Left(err), _) => Left(err)
              case (_, Left(err)) => Left(err)
              case _ => Left("less than terms must be numbers")
          case | =>
            (lh, rh) match
              case (Right(Bool(l)), Right(Bool(r))) => Right(Bool(l || r))
              case (Left(err), _) => Left(err)
              case (_, Left(err)) => Left(err)
              case _ => Left("or terms must be booleans")
      case Cx.Not(e) =>
        interpret(e, env) match
          case Right(Bool(v)) => Right(Bool(!v))
          case Right(_) => Left("not must be boolean")
          case Left(e) => Left(e)
      case Cx.If(tst, thn, els) =>
        interpret(tst, env) match
          case Right(Bool(true)) => interpret(thn, env)
          case Right(Bool(false)) => interpret(els, env)
          case Right(_) => Left("tst must be boolean")
          case Left(e) => Left(e)
      case Cx.App(fn, arg) =>
        (interpret(fn, env), interpret(arg, env)) match
          case (Right(f @ Fun(en, p, b)), Right(a)) =>
            interpret(b, en + (p -> a))
          case (Right(f @ Rec(en, n, p, b)), Right(a)) => interpret(b, en + (n -> f) + (p -> a))
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
          case (Right(_), _) => Left("cannot apply a non-function")
      case Cx.Fun(param, bdy) => Right(Fun(env, param, bdy))
      case Cx.Rec(name, param, bdy) => Right(Rec(env, name, param, bdy))
      case Cx.Empty => Right(Empty)
      case Cx.Cons(head, tail) =>
        (interpret(head, env), interpret(tail, env)) match
          case (Right(v), Right(Empty)) => Right(Cons(v, Empty))
          case (Right(v), Right(t @ Cons(_, _))) => Right(Cons(v, t))
          case (Right(v), Right(_)) => Left("tail of list must be list")
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
      case Cx.ListCase(ls, emp, h, t, cns) =>
        interpret(ls, env) match
          case Right(Cons(head, tail)) => interpret(cns, env + (h -> head) + (t -> tail))
          case Right(Empty) => interpret(emp, env)
          case Right(_) => Left("list case must be used on a list")
          case Left(err) => Left(err)
  end interpret
end Interpreter

