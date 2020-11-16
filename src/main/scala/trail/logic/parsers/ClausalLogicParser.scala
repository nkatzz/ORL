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

import trail.logic.{Clause, Constant, Literal, LogicalExpression, Variable}

import scala.util.parsing.combinator.JavaTokenParsers

class ClausalLogicParser extends JavaTokenParsers {

  def lowerCaseIdent: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  def upperCaseIdent: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  def anyWord: Parser[String] = """[A-Za-z0-9_,()+-\\#]*""".r ^^ { x => x }
  def anyWord1: Parser[String] = """[A-Za-z0-9_()+-\\#]*""".r ^^ { x => x } // no ','
  def quoted: Parser[String] = "\"" ~ anyWord ~ "\"" ^^ { case "\"" ~ x ~ "\"" => "\"" + x + "\"" } | "\'" ~ anyWord ~ "\'" ^^ { case "\'" ~ x ~ "\'" => "\'" + x + "\'" }
  def naf: Parser[String] = "not " ~ rep("\\s+") ^^ { _ => "not" }
  def iff: Parser[String] = rep("\\s+") ~ ":-" ~ rep("\\s+") ^^ { _ => ":-" }
  def number: Parser[String] = floatingPointNumber
  def quotedNumber: Parser[String] = "\"" ~ floatingPointNumber ~ "\"" ^^ { case "\"" ~ x ~ "\"" => "\"" + x + "\"" }
  def variable: Parser[LogicalExpression] = upperCaseIdent ^^ { x => Variable(x) }
  def constant: Parser[LogicalExpression] = (lowerCaseIdent | quoted) ^^ { x => Constant(x) } | (number | quotedNumber) ^^ { x => Constant(x) }
  def term: Parser[LogicalExpression] = literal | variable | constant
  def innerTerms: Parser[List[LogicalExpression]] = "(" ~> repsep(term, ",") <~ ")"
  def literal: Parser[Literal] = (
    naf ~ lowerCaseIdent ~ innerTerms ^^ { case naf ~ functor ~ inner => Literal(predSymbol = functor, terms = inner, isNAF = true) }
    | lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(predSymbol = functor, terms = inner) })
  def atom: Parser[Literal] = lowerCaseIdent ~ innerTerms ^^ { case functor ~ inner => Literal(predSymbol = functor, terms = inner) }
  def clauseHead: Parser[Literal] = atom
  def clauseBody: Parser[List[Literal]] = repsep(literal, ",")
  def clause: Parser[Clause] = clauseHead ~ iff ~ clauseBody ^^ { case head ~ iff ~ body => Clause(head, body) }

  def parseOutput(parser: Parser[LogicalExpression], expression: String): LogicalExpression = {
    getParseResult(parse(parser, expression))
  }

  def parse(parser: Parser[LogicalExpression], expression: String): Option[LogicalExpression] = {
    parseAll(parser, expression) match {
      case Success(result, _) => Some(result)
      case f => None
    }
  }

  def getParseResult(x: Option[LogicalExpression]): LogicalExpression = x match {
    case Some(y) => y
    case _ => throw new RuntimeException(x.toString)
  }

}
