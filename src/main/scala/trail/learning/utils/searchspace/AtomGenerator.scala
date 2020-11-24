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

import com.typesafe.scalalogging.LazyLogging
import trail.app.utils.CombinatorialOps._
import trail.logic.{Constant, ModeAtom, Variable}

class AtomGenerator(
    val modeAtom: String,
    val domainConstants: Map[String, Array[String]],
    val headAtom: Boolean,
    existingInputVars: List[Variable] = Nil) extends LazyLogging {

  private val pospmk = """\+[a-z][a-zA-Z0-9_]*""".r
  private val negpmk = """\-[a-z][a-zA-Z0-9_]*""".r
  private val constpmk = """\#[a-z][a-zA-Z0-9_]*""".r

  var inpVars = List.empty[Variable]
  var outpVars = List.empty[Variable]

  /**
    * Generates the cartesian product of a list of lists
    */
  def cartesianGenerator[T](x: List[List[T]]): List[List[T]] = x match {
    case Nil => List(Nil)
    case h :: t => for (j <- cartesianGenerator(t); i <- h) yield i :: j
  }

  /**
    * Returns a list of variables that correspond to the placemarker terms in the input mode atom.
    * For example, if mode = "close(+person,-person,+time)"
    *
    * Then the output of vars(mode) is
    * List(Variable(X1,+,person), Variable(X2,-,person), Variable(X3,+,time))
    *
    */
  def vars(modeAtom: String) = {
    val p = {
      if (headAtom) {
        // If we are dealing with a head atom we need to extract all its variables (which are all inpout)
        pospmk.findAllIn(modeAtom).toList ++ negpmk.findAllIn(modeAtom).toList
      } else {
        // If we're dealing with a body atom then the input variables we're operating on
        // are already given from the head atom (the body atom's input vars are just a subset of those of the
        // head. So in the body atom case we just need the body atom's output vars.
        existingInputVars.map(x => s"${x.inOrOutVar}${x._type}") ++ negpmk.findAllIn(modeAtom).toList
      }
    }
    p zip (1 to p.length) map { case (x, index) =>
      val (plmkr, ttype) = (x(0).toString, x.drop(1))
      Variable(s"X$index", plmkr, ttype)
    }
  }

  /**
    * Counts constants occurrences per type of constant. For example, if
    * mode = close(#person, #person, -person, #time)
    * then
    * constantTypeOccurrencesMap = Map(#person -> 2, #time -> 1)
    */
  def constantTypeCountsMap(modeAtom: String) = {
    constpmk.findAllIn(modeAtom).toList.groupBy(x => x).map(x => x._1 -> x._2.size)
  }

  /**
    * Use constantTypeOccurrencesMap to generate variations of constants per type. For example, if
    *
    * mode = initiatedAt(#fluent,arg(#fluent),arg(#event),+time,+person,arg(+time),-time)
    * and
    * domainConstants = Map("fluent" -> Array("alive", "dead", "loaded"), "event" -> Array("shoot", "load"))
    *
    * then constVariations look like this
    *  List(
    *    List(
    *      List(Constant(dead,#,fluent), Constant(alive,#,fluent)),
    *      List(Constant(alive,#,fluent), Constant(dead,#,fluent)),
    *      List(Constant(loaded,#,fluent), Constant(alive,#,fluent)),
    *      List(Constant(alive,#,fluent), Constant(loaded,#,fluent)),
    *      List(Constant(loaded,#,fluent), Constant(dead,#,fluent)),
    *      List(Constant(dead,#,fluent), Constant(loaded,#,fluent))
    *    ),
    *    List(
    *      List(Constant(shoot,#,event)),
    *      List(Constant(load,#,event))
    *    )
    *  )
    */
  def constantsVariations(
      constTypeCountsMap: Map[String, Int],
      domainConstantsMap: Map[String, Array[String]]) = {
    constTypeCountsMap.foldLeft(List.empty[List[List[Constant]]]) { case (accum, (key, k)) =>
      val constType = key.drop(1) // drop(1) to get rid of the "#" in front of
      val constants = domainConstantsMap(constType).toList.map(name => Constant(name, "#", constType))
      val constantsVariations = constants.xvariations(k)
      accum :+ constantsVariations
    }
  }

  /**
    * Generate atoms in the mode language (to be used in the head/body of rules) from a mode atom specification.
    * We need to discriminate between head atoms and body atoms and handle them differently (hence headAtom parameter).
    *
    * if we are dealing with a body atom, then the permutations of the variables need to be taken into account
    * (these variables are taken from the head, or from other body atoms that have appeared previousely and
    * contain output placemarkers). For example, if we have
    *
    * body(close(+person,+person,#distance,+time))
    *
    * and the input variables X1:person, X2:person, X3:person, then from the mode specification above we should generate:
    *
    * close(X1,X2,40,X3) and close(X2,X1,40,X3)
    *
    * On the other hand, if we're dealing with a head atom then the permutations of the variables are irrelevant.
    * For example, from
    *
    * head(initiatedAt(meet(+person,+person),+time))
    *
    * only one actual head atom will be generated, namely
    *
    * initiatedAt(meet(X1,X2),X3)
    *
    * But, as in body atoms, in head atoms the permutations of CONSTANTS should be take into account and each
    * permutation generates a different head (so with more than one constants of the same type in the head more than
    * one bottom rules will be generated, each for each permutation). For example from
    *
    * head(initiatedAt(blocks(#protein,#protein),+time))
    *
    * and the BK protein(pr_1), protein(pr_1) two bottom rules should be generated, with heads:
    *
    * initiatedAt(blocks(pr_1,pr_2),X1)
    * initiatedAt(blocks(pr_2,pr_1),X1)
    *
    */
  def generate = {

    val variables = vars(modeAtom)

    val (_inpVars, _outpVars) = variables.partition(_.inOrOutVar == "+")
    if (headAtom) inpVars = _inpVars // if bodyAtom no new input vars are generated
    if (headAtom && _outpVars.nonEmpty) {
      //logger.error(s"Output variables in head atom: $modeAtom")
      throw new RuntimeException(s"Output variables in head atom: $modeAtom")
    }
    outpVars = _outpVars

    val constantTypeOccurrencesMap = constantTypeCountsMap(modeAtom)
    val constVariations = constantsVariations(constantTypeOccurrencesMap, domainConstants)
    val varVariations = if (!headAtom) variables.xvariations(variables.length) else List(variables)

    // put them all together
    val varsPlusConsts = (constVariations :+ varVariations).reverse

    // ...and generate their cartesian product. Each tuple in the cartesian product is a substitution, e.g.
    // List(Variable(X4,-,time), Variable(X3,+,time), Variable(X2,+,person), Variable(X1,+,time), Constant(shoot,#,event), Constant(dead,#,fluent), Constant(alive,#,fluent))
    // List(Variable(X4,-,time), Variable(X3,+,time), Variable(X1,+,time), Variable(X2,+,person), Constant(shoot,#,event), Constant(dead,#,fluent), Constant(alive,#,fluent))
    // List(Variable(X4,-,time), Variable(X1,+,time), Variable(X3,+,time), Variable(X2,+,person), Constant(shoot,#,event), Constant(dead,#,fluent), Constant(alive,#,fluent))
    // ...
    val cartesianProduct = cartesianGenerator(varsPlusConsts).map(_.flatten)

    // Finally this applies the substitutions to the mode atom and generates the actual atoms in the mode language.
    val generatedAtoms = cartesianProduct.foldLeft(List.empty[String]) { (atomsAccumulator, substitution) =>
      val generatedAtom = substitution.foldLeft(modeAtom) { (partialSubstsAtom, termToReplace) =>
        val (termType, actualTerm) = {
          if (termToReplace.isVariabe) {
            val variable = termToReplace.asInstanceOf[Variable]
            (s"\\${variable.inOrOutVar}${variable._type}", variable.name)
          } else {
            val constant = termToReplace.asInstanceOf[Constant]
            (s"#${constant._type}", constant.name)
          }
        }
        val newAtom = partialSubstsAtom.replaceFirst(termType, actualTerm)
        newAtom
      }
      atomsAccumulator :+ generatedAtom
    }
    generatedAtoms
  }

  // These should be input
  /*val modeAtom = "initiatedAt(#fluent,arg(#fluent),arg(#event),+time,+person,arg(+time),-time)"
  val domainConstants = Map("fluent" -> Array("alive", "dead", "loaded"), "event" -> Array("shoot", "load"))

  val t = generateAtoms(modeAtom, domainConstants, false)

  t foreach println*/

  def modesDrivenBottomRules(modehs: List[ModeAtom], modebs: List[ModeAtom], domainConstants: Map[String, Array[String]]) = {
      def generateBR(headMode: ModeAtom) = {}
  }

}
