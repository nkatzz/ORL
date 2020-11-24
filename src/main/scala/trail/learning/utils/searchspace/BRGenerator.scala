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

package trail.learning.utils.searchspace

import trail.app.runutils.RunningOptions
import trail.logic.{Clause, ModeAtom, Variable}

object BRGenerator {

  def generateBRs(modehs: List[ModeAtom], modebs: List[ModeAtom],
      domainConstants: Map[String, Array[String]]): List[Clause] = {

    modehs.flatMap(x => generateBRs(x, modebs, domainConstants))
  }

  def generateBRs(modeh: ModeAtom, modebs: List[ModeAtom],
      domainConstants: Map[String, Array[String]]): List[Clause] = {

    var inputVars = List.empty[Variable]
    val headAtomGenerator = new AtomGenerator(modeh.totext, domainConstants, true)

    /**
      * Generate different variants of allowed head atoms.
      * These could be more than one, (see the comment preceding the
      * generate method in AtomGenerator) and one bottom rule per such
      * head will be generated.
      */
    val headAtoms = headAtomGenerator.generate

    /**
      * After generating the head atoms the input vars extracted from the mode head atom template
      * will be the "seed" input variables to use for the body atoms.
      */
    inputVars = headAtomGenerator.inpVars

    /**
      * Generate body atoms.
      */
    val maxIterations = 2
    var counter = 0
    var batoms = Set.empty[String]

    while (counter < maxIterations) {
      val _batoms = modebs.foldLeft(Set.empty[String]) { (atoms, matom) =>
        val mAtomString = matom.totext
        val bodyAtomGenerator = new AtomGenerator(mAtomString, domainConstants, false, inputVars)
        val bodyAtoms = bodyAtomGenerator.generate

        // Augment the initial input variables with output variables extracted from body atoms.
        // These extra variable will be used as input for subsequent body template atoms.
        val extractedOutputVars = bodyAtomGenerator.outpVars
        val newInputVars = extractedOutputVars.map(outvar => Variable(outvar.name, "+", outvar._type))
        inputVars = inputVars ++ newInputVars
        atoms ++ bodyAtoms
      }
      batoms = batoms ++ _batoms
      counter += 1
    }



    val _bottomRules = headAtoms.map(x => s"$x :- ${batoms.mkString(",")}")
    val bottomRules = _bottomRules.map(x => Clause.parseWPB2(x))
    bottomRules.foreach(x => x.setTypeAtoms(List(modeh) ++ modebs))
    bottomRules
  }

}
