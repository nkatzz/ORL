/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package trail.logic.parsers

import com.typesafe.scalalogging.LazyLogging
import org.parboiled2._
import trail.logic.{Constant, Literal, LogicalExpression, Variable}

import scala.util.{Failure, Success, Try}

object PB2LogicParser extends LazyLogging {

  def parseInputData(input: String, debug: Boolean = false) = {
    val data = input.split("\n").filter(x => x.nonEmpty && !x.startsWith("%")).mkString(" ")
    val parser = new PB2LogicParser(data)
    val result = parser.inputData.run()
    val out = result match {
      case Success(inputAsAtoms) => Some(inputAsAtoms)
      case Failure(e: ParseError) =>
        logger.error(parser.formatError(e)); None
      case Failure(e: Throwable) => throw e
    }
    out match {
      case Some(x) => x
      case _ => throw new RuntimeException
    }
  }

  def parseClause(c: String, debug: Boolean = false): LogicalExpression = {
    val parser = new PB2LogicParser(c)
    val result = parser.Clause.run()
    getParserResult(result, parser, debug)
  }

  def parseAtom(a: String, debug: Boolean = false): LogicalExpression = {
    val parser = new PB2LogicParser(a)
    val result = parser.Atom.run()
    getParserResult(result, parser, debug)
  }

  private def getParserResult(result: Try[LogicalExpression], parser: PB2LogicParser, debug: Boolean = false) = {
    val out = result match {
      case Success(x) =>
        if (debug) {
          logger.info("\n" + x.tostring)
          Some(x)
        } else Some(x)
      case Failure(e: ParseError) =>
        logger.error(parser.formatError(e)); None
      case Failure(e: Throwable) => throw e
    }
    out match {
      case Some(x) => x
      case _ => throw new RuntimeException
    }
  }

}

final class PB2LogicParser(val input: ParserInput) extends Parser {

  case class ExpressionList(elems: List[LogicalExpression])

  def Clause = rule {
    Atom ~ iff ~ BodyLiterals ~ optional(".") ~ EOI ~> ((x, y) =>
      trail.logic.Clause(head = x, body = y.elems.map(_.asInstanceOf[Literal]))) |
      Atom ~ optional(".") ~ EOI ~> (x => trail.logic.Clause(head = x, body = Nil)) // allow for empty-bodied clauses.
  }

  def WeightedClause = rule {
    Atom ~ " :- " ~ BodyLiterals ~ optional(".") ~ EOI ~> ((x, y) =>
      trail.logic.Clause(head = x, body = y.elems.map(_.asInstanceOf[Literal])))
  }

  def Atom = rule {
    Funct ~ InnerTerms ~ optional(".") ~> ((x, y) => Literal(predSymbol = x, terms = y.elems)) |
      "not " ~ Funct ~ InnerTerms ~ optional(".") ~> ((x, y) => Literal(predSymbol = x, terms = y.elems, isNAF = true))
  }

  /*
  * Parsing input in order to discriminate between observation & query atoms.
  *
  * */
  /*def inputData = rule {
    //oneOrMore(Atom).separatedBy(whiteSpace) ~ optional(".") ~ EOI ~> (x => x.toSet)
    oneOrMore(Atom).separatedBy( zeroOrMore(whiteSpace) | EOI ) ~ EOI ~> (x => x.toSet)
  }*/

  def inputData = rule {
    optional(whiteSpace) ~ oneOrMore(Atom).separatedBy(whiteSpace) ~ optional(EOI) ~> (x => x.toSet)
  }

  private def Term: Rule1[LogicalExpression] = rule { Atom | Const | Var }

  private def BodyLiterals = rule {
    oneOrMore(Atom).separatedBy(InnerSeparator) ~> (x => ExpressionList(x.toList))
  }

  private def InnerTerms = rule {
    whiteSpace ~ "(" ~ whiteSpace ~ oneOrMore(Term).
      separatedBy(InnerSeparator) ~ whiteSpace ~ ")" ~ whiteSpace ~> (x => ExpressionList(x.toList))
  }

  private def Funct = rule {
    (capture(LowerCaseString) ~> ((x: String) => x)) | (capture(ClassicalNegation) ~> ((x: String) => x))
  }

  private def Var = rule { capture(UpperCaseString) ~> ((x: String) => Variable(x)) }

  private def Const = rule {
    (capture(LowerCaseString) ~> ((x: String) => Constant(x))) |
      (capture(Integer) ~> (x => Constant(x))) |
      (capture(MinusInteger) ~> (x => Constant(x))) |
      (capture(optional('"') ~ TK_WhatEver ~ optional('"')) ~> (x => Constant(x))) |
      (capture(optional('"') ~ LowerCaseString ~ optional('"')) ~> ((x: String) => Constant(x))) |
      (capture('"' ~ UpperCaseString ~ '"') ~> ((x: String) => Constant(x)))
  }

  private def InnerSeparator = rule { whiteSpace ~ "," ~ whiteSpace }

  private def WhiteSpaceChar = CharPredicate(" ")

  private def whiteSpace = rule(zeroOrMore(WhiteSpaceChar))

  private def iff = rule { whiteSpace ~ ":-" ~ whiteSpace }

  private def LowerCaseString = rule { CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") }

  private def ClassicalNegation = rule { "-" ~ CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") }

  private def Integer = rule { oneOrMore(CharPredicate.Digit) }

  private def TK_WhatEver = rule { Integer ~ "_" ~ Integer }

  // This is needed in use/2 atoms with rule ids, e.g. use(-23421, 0)
  private def MinusInteger = rule { "-" ~ oneOrMore(CharPredicate.Digit) }

  private def UpperCaseString = rule { CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") }

}

