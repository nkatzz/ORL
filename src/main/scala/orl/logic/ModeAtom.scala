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

package orl.logic

/**
  * Created by nkatz at 4/12/19
  */

object ModeAtom {

  val comparisonRelationGreaterThan = "greaterThan"
  val comparisonRelationLessThan = "lessThan"
  //val modeHeadSymbol = "modeh"
  //val modeBodySymbol = "modeb"

  def apply(): ModeAtom = {
    ModeAtom("", Nil)
  }
}

case class ModeAtom(predSymbol: String, args: List[LogicalExpression], isNAF: Boolean = false) extends LogicalExpression {

  val arity: Int = this.args.length

  val isEmpty: Boolean = args match {
    case List() => true
    case _ => false
  }

  var compRelation = ""
  var comparisonTermPosition: List[Int] = List[Int]()

  def comparisonTermType = getComparisonTerm._type

  def getComparisonTerm: LogicalExpression = {
    if (comparisonTermPosition.nonEmpty) {
      val first = args(comparisonTermPosition.head - 1)
      val rest = comparisonTermPosition.tail
      if (rest.nonEmpty) rest.foldLeft(first)((term, position) => term.asInstanceOf[ModeAtom].args(position - 1)) else first
    } else Constant()
  }

  def isComparisonPredicate = {
    compRelation == ModeAtom.comparisonRelationLessThan || compRelation == ModeAtom.comparisonRelationGreaterThan
  }

  /**
    * @return a string representation of the mode declaration. This method is supposed to be called on a
    * variabilized version of the mode declaration, and it surrounds with double quotes
    * variables that correspond to output and ground placemarkers. For instance, assume the mode atom
    *
    * modeb(p(+type1,-type2,#type3))
    *
    * and its variabilized version
    *
    * p(X,Y,Z)
    *
    * The result of applying this method on the above is
    *
    * p(X,"Y","Z"). These atoms are passed to the ASP solver, in order to generate query atoms, for the
    * construction of the body of a Kernel Set clause. The quoted variables are treated as constants by the
    * solver, which generates instances by grounding only the variables that correspond to input terms.
    * The quotes are removed by post-processing each atom in an answer set, thus obtaining a query atom (i.e.
    * an atom of the form p(2,Y,Z) from the above). This is a query atom, which is subsequently used to
    * generate groundings of the atom that bind only Y,Z vars, keeping the input term intact.
    *
    */

  override lazy val tostring: String = args match {
    case List() => if (isNAF) s"not $predSymbol" else predSymbol
    case _ => if (isNAF) s"not $predSymbol" else predSymbol + "(" + (for (a <- args) yield a.tostring).mkString(",") + ")"
  }

  /**
    * Variabilizes a mode declaration atom, i.e. it replaces all in-out-ground placemarkers with fresh variables.
    * The variabilized mode declarations are used in the construction of bottom clauses, in order to generate ground
    * instances of mode declarations atoms, by replacing variables by constants found in the data.
    *
    * returns a variabilized Literal. It's variables are annotated as +/-/# and it also carries a List[string] with the
    * typing predicates for it's variables.
    *
    */
  def varbed: Literal = {
    val (varbed, ttypes, _) = variabilize(List(Literal(predSymbol = this.predSymbol)), this.args, List(), 0)
    Literal(predSymbol = varbed.head.predSymbol, terms = varbed.head.terms, isNAF = this.isNAF, typePreds = ttypes, modeAtom = this)
  }

  /**
    *
    * This method does all the work of the variabilation.
    *
    * @param accum an accumulator that collects competed (variabilized) compound sub-terms.
    * @param remaining a list containing all remaining sub-terms that should be variabilized.
    * @param ttypes a list collecting typing predicates for the generated variables, e.g. person(X1), time(X100)
    * @param counter a counter that is incremented by 1 each time a new variable is generated. The name of a new variable is
    * simply "X"+currentCounterValue.
    */
  private def variabilize(accum: List[Literal], remaining: List[LogicalExpression],
      ttypes: List[String], counter: Int): (List[Literal], List[String], Int) = {
      def f(x: LogicalExpression, sign: String, tail: List[LogicalExpression]) = {
        val cur = accum match {
          case Nil => Literal(predSymbol = this.predSymbol)
          case _ => accum.last
        }
        // We are variabilizing everything (it's modes variabilization) so replace all with a new Var.
        val update = Literal(predSymbol = cur.predSymbol, terms = cur.terms :+ Variable("X" + counter, sign, x._type))
        this.variabilize(accum.tail :+ update, tail, ttypes :+ x._type + "(X" + counter + ")", counter + 1)
      }
    remaining match {
      case head :: tail => head match {
        case x: PlmrkPos => f(x, "+", tail)
        case x: PlmrkNeg => f(x, "-", tail)
        case x: PlmrkConst => f(x, "#", tail)
        case x: ModeAtom =>
          val (varbed, newTypes, newCount) = this.variabilize(List(Literal(predSymbol = x.predSymbol)), x.args, List(), counter)
          val pop = accum.last
          this.variabilize(List(Literal(predSymbol = pop.predSymbol, terms = pop.terms ::: varbed)), tail, ttypes ::: newTypes, newCount)
        case _ =>
          throw new RuntimeException("Variabilizing Mode Declaration " + this.tostring + ": Found unexpected type")
      }
      case Nil =>
        val pop = accum.last
        (accum.tail :+ Literal(predSymbol = pop.predSymbol, terms = pop.terms), ttypes, counter)
    }
  }

}

