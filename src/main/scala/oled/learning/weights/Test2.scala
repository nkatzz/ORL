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

package oled.learning.weights

import oled.logic.{Clause, Literal}

/**
  * Created by nkatz at 11/2/20
  */
object Test2 {

  def transform(rules: List[Clause]) = {

    val minDiff = rules.map(_.weight).toSet.subsets(2).map(pair => math.abs(pair.head - pair.tail.head)).toVector.min
    val scaleFactor = 200.0 / minDiff
    val rulesWithintWeights = rules.map(x => (x, x.weight)).map(x => (x._1, math.round(x._2 * scaleFactor)))

    val all = rulesWithintWeights zip (1 to rules.length) map { case ((rule, intWeight), ruleId) =>
      val satAtom = Literal.parse(s"satisfied(${rule.head.tostring},rule_$ruleId)")
      val typePreds = rule.getVars.map(x => Literal.parse(s"${x._type}(${x.name})"))
      val satClause = Clause(head = rule.head, body = List(satAtom) ++ typePreds).tostring
      val choiceRule = s"{${satAtom.tostring}} :- ${typePreds.map(_.tostring).mkString(",")}."
      val weakConstraint = {
        if (rule.weight >= 0.0) {
          s":~ not ${satAtom.tostring}, ${rule.body.map(_.tostring).mkString(",")}. [$intWeight,${rule.getVars.map(_.name).mkString(",")}]"
        } else {
          s":~ ${satAtom.tostring}, ${rule.body.map(_.tostring).mkString(",")}. [${-intWeight},${rule.getVars.map(_.name).mkString(",")}]"
        }
      }
      s"$satClause\n$choiceRule\n$weakConstraint\n"
    }
    all.mkString("\n")
  }
}

object Test3 extends App {
  def precision(tps: Int, fps: Int) = tps.toDouble / (tps + fps)
  def recall(tps: Int, fns: Int) = tps.toDouble / (tps + fns)
  def f1(tps: Int, fps: Int, fns: Int) = {
    val p = precision(tps, fps)
    val r = recall(tps, fps)
    2 * p * r / (p + r)
  }
  println(f1(3065, 514, 577))
}
