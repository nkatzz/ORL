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

package orl.learning.woledasptr

import orl.logic.{Clause, Literal}

class Revisor(val existingRules: List[Clause], val bcs: List[Clause]) {

  def getTypePredicates(lit: Literal) = {
    lit.variables.map(x => Literal.parse(s"${x._type}(${x.name})"))
  }

  /**
    * If existing is true then the rule is revised by adding or removing literals.
    * Otherwise the rule is a bottom clause and the transformation aims to rule induction.
    */
  def transformRule(rule: Clause, existing: Boolean = false) = {

    val useTryExceptionAtoms = (rule.body zip (1 to rule.body.length)).map{ case (literal, literalId) =>
      val variablesTerm = s"vars(${literal.getVars.map(x => x.name).mkString(",")})"
      val tryAtom = s"try($variablesTerm, $literalId, ${rule.##})"
      val useAtom = s"use($literalId,${rule.##})"
      val typePreds = getTypePredicates(literal).map(_.tostring).mkString(",")
      (tryAtom, useAtom, literal.tostring, typePreds)
    }

    val variablesTerm = s"vars(${rule.getVars.map(x => x.name).mkString(",")})"
    val exceptionAtom = s"excpt($variablesTerm, ${rule.##})"

    val useClause = {
      if (existing) {
        s"${rule.head.tostring} :- " +
          s"use(0,${rule.##}),${useTryExceptionAtoms.map(x => x._1).mkString(",")},not ${exceptionAtom},${rule.typeAtoms.mkString(",")}.\n"
      } else {
        s"${rule.head.tostring} :- " +
          s"use(0,${rule.##}),${useTryExceptionAtoms.map(x => x._1).mkString(",")},${rule.typeAtoms.mkString(",")}.\n"
      }
    }

    /**
      * We need try/3 definitions for each body
      */

    val tryClauses = {
      if (existing) {

      }
    }
  }

  /*def metaProgram = {

    existingRules map { rule =>

      val useTryExceptionAtoms = (rule.body zip (1 to rule.body.length)).map{ case (literal, literalId) =>
        val variablesTerm = s"vars(${literal.getVars.map(x => x.name).mkString(",")})"
        val tryAtom = s"try($variablesTerm, $literalId, ${rule.##})"
        val useAtom = s"use($literalId,${rule.##})"
        val exception =
        val typePreds = getTypePredicates(literal).map(_.tostring).mkString(",")
        (tryAtom, useAtom, literal.tostring, typePreds)
      }

    }
  }*/

}
