package com.dallinhuff.banana

package com.dallinhuff.banana

import cats.parse.Parser as P
import P.{char, charWhere, string}
import cats.parse.Rfc5234.{alpha, digit, wsp}

import Types.SurfaceExpression
import Types.SurfaceExpression.*
import Types.SurfaceOp
import Types.SurfaceOp.*

object Parser:
  extension [A, B, C](p: P[(A, B)])
    inline infix def ~~(o: P[C]): P[(A, B, C)] = (p ~ o).map(i => (i(0)(0), i(0)(1), i(1)))

  extension [A](p: P[A])
    inline def wss: P[A] = p.surroundedBy((wsp | char('\n')).rep0)

  def parse(str: String): Either[String, SurfaceExpression] =
    expr.parseAll(str).fold(
      err => Left(s"'${err.input.getOrElse("")}' is not valid syntax"),
      expr => Right(expr)
    )

  private val expr: P[SurfaceExpression] = P.recursive: expr =>
    val num = (digit.rep ~ (char('.') *> digit.rep).?).map:
      case (intPart, Some(decPart)) =>
        Num(((intPart :+ '.') ++ decPart.toList).toList.mkString.toDouble)
      case (intPart, None) =>
        Num(intPart.toList.mkString.toDouble)

    val bool =
      (string("#t") | string("#true")).map[Bool](_ => Bool(true)) |
      (string("#f") | string("#false")).map[Bool](_ => Bool(false))

    val ident: P[String] =
      val identStart = alpha | charWhere("-_?!@$%^&*+/<>=".contains)
      (identStart ~ (identStart | digit).rep0).string

    val atom: P[SurfaceExpression] = num | bool | ident.map(Ident.apply)

    def paren[T](inner: P[T]): P[T] =
      inner.wss.between(char('('), char(')')) |
      inner.wss.between(char('['), char(']')) |
      inner.wss.between(char('{'), char('}'))

    def comp[T](keyword: String, args: P[T]): P[T] = paren(string(keyword) *> args).backtrack

    val notE = comp("not", wss(expr)).map(Not.apply)
    val ifE = comp("if", expr.wss ~ expr.wss ~~ expr.wss).map(If.apply)

    val letE = comp("let", paren(paren(ident.wss ~ expr.wss).wss).wss ~~ expr.wss).map(Let.apply)
    val appE = comp("app", expr.wss ~ expr.wss).map(App.apply)

    val funE = comp("fun", paren(ident).wss ~ expr.wss).map(Fun.apply)
    val recE = comp("rec", ident.wss ~ paren(ident).wss ~~ expr.wss).map(Rec.apply)

    val emptyE = paren(string("empty")).map(_ => Empty).backtrack
    val consE = comp("cons", expr.wss ~ expr.wss).map(Cons.apply)

    val listCaseE: P[ListCase] =
      val emptyCase = paren(emptyE *> expr.wss)
      val consDestruct = comp("cons", ident.wss ~ ident.wss)
      val consCase = paren(consDestruct.wss ~~ expr.wss)

      comp("list-case", expr.wss ~ emptyCase.wss ~ consCase.wss).map:
        case ((ls, emp), (h, t, cns)) => ListCase(ls, emp, h, t, cns)

    val binE: P[Bin] =
      val op: P[SurfaceOp] =
        char('+').map(_ => +) |
        char('-').map(_ => -) |
        char('*').map(_ => *) |
        char('/').map(_ => /) |
        char('=').map(_ => ===) |
        string("<>").map(_ => <>) |
        string("<=").map(_ => <=) |
        string(">=").map(_ => >=) |
        char('<').map(_ => <) |
        char('>').map(_ => >) |
        string("and").map(_ => &) |
        string("or").map(_ => |)

      paren(op.wss ~ expr.wss ~~ expr.wss).map(Bin.apply)

    atom | notE | ifE | letE | appE | funE | recE | emptyE | consE | listCaseE | binE
  end expr
end Parser

