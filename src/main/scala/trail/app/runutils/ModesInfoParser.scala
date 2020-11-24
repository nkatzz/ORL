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

package trail.app.runutils

import com.typesafe.scalalogging.LazyLogging
import trail.logic.parsers.ModesParser
import trail.logic.{Literal, ModeAtom, Variable}

import scala.io.Source
import scala.util.matching.Regex

/**
  * This is called from within the Globals class once the app starts. An instance
  * of this class is kept in the Globals instance stored in the RunningOptions instance,
  * which is passed-around between methods, in order to provide access to global variables
  * during run-time.
  *
  * The ModesInfoParser provides access to:
  * - mode declarations (templates for constructing rules in the mode language).
  * - example patterns (templates of 'label' atoms - the annotation for learning and target predicates for inference)
  * - type axioms, which extract 'definitions' for the mode declarations' types and are used by the ASP solver for grounding.
  * - input predicates, which are provided by the used and are used to extract the type axioms.
  *
  *
  */

class ModesInfoParser(modesFile: String) extends LazyLogging {

  def matches(p: Regex, str: String) = p.pattern.matcher(str).matches

  def getTypeAxioms(m: Literal): Set[String] = {
    val plmrkTerms = m.placeMarkers
    val (posPlmrkTerms, negPlrmTerms, grndPlmrkTerms) = (plmrkTerms._1, plmrkTerms._2, plmrkTerms._3)
    val allPlmrks = (posPlmrkTerms ++ negPlrmTerms ++ grndPlmrkTerms).map(x => x.asInstanceOf[Variable]).toSet

    allPlmrks.foldLeft(Set[String]()) { (accum, y) =>
      val allOtherPlmrks = allPlmrks diff Set(y)
      if (y.inOrOutVar == "+" || y.inOrOutVar == "-") {
        val result_ = s"${y._type}(${{ y.name }}) :- ${m.tostring}."

        // the regex below matches variable symbols which do not appear in predicate of function
        // names. So it will match X0 in p(X0) but not in pX0(X0), pxX0(X0), pX_0(X0), p_2X0(X0) and so on
        val result = allOtherPlmrks.foldLeft(result_) { (x1, y1) => x1.replaceAll(s"(?<![a-zA-Z0-9_]+)${y1.name}", "_") }
        accum + result
      } else {
        accum
      }
    }
  }

  private val modesParser = new ModesParser

  val modes: List[String] =
    if (modesFile.startsWith("None")) Nil
    else {
      val source = Source.fromFile(modesFile)
      val content = source.getLines.toList.filter(line => !matches("""""".r, line) && !line.startsWith("%"))
      source.close()
      content
    }

  val headModes: List[ModeAtom] =
    modes.filter(m => (m.startsWith("modeh") || m.startsWith("head")) && !m.startsWith("%")).map(x => x).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeh, x)))

  val bodyModes: List[ModeAtom] =
    modes.filter(m => (m.startsWith("modeb") || m.startsWith("body")) && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.modeb, x)))

  if (headModes.isEmpty) {
    logger.error("No head mode declarations found.")
    System.exit(-1)
  }

  if (bodyModes.isEmpty) {
    logger.error("No body mode declarations found.")
    System.exit(-1)
  }

  val exmplPatterns: List[ModeAtom] = {

    val eps = modes.filter(m => m.startsWith("examplePattern") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.exmplPattern, x)))

    if (eps.isEmpty) headModes else eps // just use the heads as target predicates.
  }

  // Auxiliary predicates. These are input predicates which are not part of the target language
  // but are necessary for extracting the types of entities in the domain (e.g. think of coords/4 in CAVIAR).
  val inputPreds: List[ModeAtom] =
    modes.filter(m => m.contains("inputPredicate") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.inputPred, x)))

  if (inputPreds.exists(p => p.isNAF)) {
    logger.error(s"NAF is not allowed in input predicates.")
    System.exit(-1)
  }

  // This method generates types axioms for the mode declarations,
  // i.e. rules of the form: time(X1) :- happensAt(active(_),X1).
  val typeAxioms: Set[String] = {
    //val m = inputPreds.filter(x => !x.isNAF).map(x => x.varbed)
    /**
      * Using input predicates seems redundant. Just extract the type definitions from the body declarations.
      * If it turns out the input predicates are necessary just revert this, nothing has been modified.
      */
    val m = bodyModes.filter(x => !x.isNAF).map(x => x.varbed)
    val x = m.flatMap(getTypeAxioms).toSet
    x
  }

  /*
  * Comparison predicates compare numerical values to a threshold, e.g:
  *
  * close(p1, p2, 30, 10)
  *
  * meaning that the Euclidean distance of p1, p2 at time 10 is less than 30.
  *
  * Comparison predicates may be declared in the modes file like this:
  *
  * comparisonPredicate(close(+person,+person,#numvalue,+time), lessThan, comparison_term_position(3))
  *
  * The #numvalue placemarker indicates the position of the actual numerical threshold
  * while the 'lessThan' term (can also be 'greaterThan') declares the intended "semantics"
  * of the predicate. Note that numvalue has to be the type of this term in the corresponding body declaration. The
  * comparison_term_position(3) indicates the position of the comparison term in the atom. In folded atoms the whole
  * "path" to this term needs to be specified e.g.
  *
  * comparisonPredicate(far(+person,+person,test(+person, p(#threshold_value)),+time), greaterThan, comparison_term_position(3,2,1))
  *
  * Here to find the comparison term take atom.terms(3).terms(2).terms(1). See also the method getComparisonTerm
  * in the Modes class and the getComparisonTerm in the Literal class.
  *
  * Comparison predicate declarations are used internally to allow for two tasks that simplify the learning process:
  *
  * 1. Reduce clauses: When a comparison predicate in the lessThan semantics and with numvalue1 is added to a rule,
  *    then any other similar predicate with numvalue2 such that numvalue2 > numvalue1 is removed from the rule.
  *    Rules with comparison predicate in the greaterThan semantics are reduced accordingly.
  * 2. When generating candidate specializations, rules that consist of comparison predicates only (e.g. close/4
  *    predicates only) are omitted.
  * */
  val comparisonPredicates: List[ModeAtom] = {
    modes.filter(m => m.contains("comparisonPredicate") && !m.startsWith("%")).
      map(x => modesParser.getParseResult(modesParser.parseModes(modesParser.compPred, x)))
  }

  bodyModes foreach { m =>
    val x = comparisonPredicates.find(z => z == m).getOrElse(ModeAtom())
    if (x != ModeAtom()) {
      m.compRelation = x.compRelation
      m.comparisonTermPosition = x.comparisonTermPosition
    }
  }
}
