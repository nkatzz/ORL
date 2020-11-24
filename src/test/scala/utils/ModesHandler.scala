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

package utils

import trail.logic.{Literal, ModeAtom}
import trail.logic.parsers.ModesParser

import scala.util.matching.Regex

object ModesHandler {

  val modesParser = new ModesParser
  def matches(p: Regex, str: String) = p.pattern.matcher(str).matches

  /**
    * This is identical to
    */
  def getModes(modeDeclarationsSource: String) = {

    val MODES = {
      val source = modeDeclarationsSource.split("\n")
      val content = source.filter(line => !matches("""""".r, line) && !line.startsWith("%"))
      content.toList
    }

    val MODEHS: List[ModeAtom] = MODES.filter(m => m.contains("modeh") && !m.startsWith("%")).map(x => x).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeh, x)))

    val MODEBS: List[ModeAtom] = MODES.filter(m => m.contains("modeb") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeb, x)))

    val exmplPatterns_1: List[ModeAtom] =
      MODES.filter(m => m.contains("examplePattern") && !m.startsWith("%")).
        map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.exmplPattern, x)))

    val exmplPatterns_2: List[ModeAtom] = exmplPatterns_1 match {
      case List() => MODEHS // if no example patterns are found, use the head mode declarations for them
      case _ => exmplPatterns_1
    }

    val exmplPatterns: List[ModeAtom] = exmplPatterns_2

    val inputPreds: List[ModeAtom] = {
      MODES.filter(m => m.contains("inputPredicate") && !m.startsWith("%")).
        map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.inputPred, x)))
    }

    (MODEHS, MODEBS, exmplPatterns, inputPreds)
  }

}
