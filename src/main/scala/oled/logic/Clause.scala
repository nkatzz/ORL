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

package oled.logic

import java.text.DecimalFormat
import java.util.UUID

import oled.logic.parsers.PB2LogicParser

import scala.collection.mutable.ListBuffer

/**
  * Created by nkatz at 4/12/19
  */

object Clause {
  val empty: Clause = Clause()

  def apply(lits: List[Literal]) = {
    new Clause(head = lits.head, body = lits.drop(1))
  }

  def apply(head: Literal, body: List[Literal]) = {
    new Clause(head = head, body = body)
  }

  /* Parses a string into a Clause. */
  /*
  def parse(cl: String): Clause = {
    val p = new ClausalLogicParser
    p.getParseResult(p.parse(p.clause, cl)).asInstanceOf[Clause]
  }
  */

  /* Use this which is faster that combinators parsing. If any problems occur just fall back to the previous parser
  * (uncomment the method above.) See also the related comment at the parse method of the Literal companion object.*/
  def parse(cl: String) = parseWPB2(cl)

  def parseWPB2(cl: String) = PB2LogicParser.parseClause(cl).asInstanceOf[Clause]

  def toMLNFlat(c: Clause) = {}

}

case class Clause(
    head: Literal = Literal(),
    body: List[Literal] = Nil,
    uuid: String = UUID.randomUUID.toString) extends LogicalExpression {

  var parentClause: Clause = Clause.empty
  var isBottomRule = false
  var isTopRule = false
  var weight: Double = 0.0001
  var subGradient: Double = 0.0
  var mistakes: Double = 0.0

  var adamGradient = 0.0
  var adamSquareSubgradient = 0.0

  var tps: Int = 0
  var fps: Int = 0
  var fns: Int = 0
  var tns: Int = 0
  var refinements = List.empty[Clause]
  var seenExmplsNum = 0 // The number of examples until the Hoeffding test succeeds
  var supportSet: List[Clause] = Nil

  lazy val length: Int = this.body.length + 1

  def format(x: Double) = {
    val defaultNumFormat = new DecimalFormat("0.############")
    defaultNumFormat.format(x)
  }

  def addToSupport(c: Clause) = this.supportSet = this.supportSet :+ c

  def addToSupport(c: List[Clause]) = this.supportSet = this.supportSet ++ c

  def removeFromSupport(c: Clause) = this.supportSet = this.supportSet.filter(x => x != c)

  def compressSupport = {
    val redundants = this.supportSet filter {
      p =>
        this.supportSet exists {
          q => !p.equals(q) && (p thetaSubsumes q)
        }
    }
    this.supportSet = this.supportSet filter (p => !redundants.contains(p))
  }

  def showWithStats(scoreFun: String, showWeights: Boolean = true) = {
    if (showWeights) {
      s"Precision:" + s" ${this.precision}, TPs: $tps, FPs: $fps, FNs: $fns | " +
        s"weight: ${format(this.weight)}  " + s"Evaluated on: ${this.seenExmplsNum} examples\n$tostring"
    } else {
      s"Precision:" + s" ${this.precision}, TPs: $tps, FPs: $fps, FNs: $fns. " + s"Evaluated on: ${this.seenExmplsNum} examples\n$tostring"
    }
  }

  def showWithStatsFormal(scoreFun: String, showWeights: Boolean = true) = {
    if (showWeights) {
      s"Precision:" + s" ${this.precision}, TPs: $tps, FPs: $fps, FNs: $fns | " +
        s"weight: ${format(this.weight)}  " + s"Evaluated on: ${this.seenExmplsNum} examples\n$tostringFormal"
    } else {
      s"Precision:" + s" ${this.precision}, TPs: $tps, FPs: $fps, FNs: $fns. " + s"Evaluated on: ${this.seenExmplsNum} examples\n$tostringFormal"
    }
  }

  def thetaSubsumes(that: Clause): Boolean = {

      def isSubset(x: Set[Any], y: Set[Any]): Boolean = x subsetOf y

    val isVar = (x: String) => try { Variable(x); true } catch { case _: IllegalArgumentException => false }

    val (skolemised, skmap) = that.skolemise
    var skolems = (for (y <- skmap.keySet.filter(x => isVar(x))) yield skmap(y)).toList
    val thisVars = this.getVars
    while (thisVars.length > skolems.length) {
      skolems = skolems ::: skolems
    }
    for (x <- skolems.permutations) {
      val trySubstitution = (thisVars zip x).map { x => (x._1, Constant(x._2)) }.toMap
      val repl = this.toLiteralList.map { x => x.replaceAll(trySubstitution) }.map { x => x.tostring }
      if (isSubset(repl.toSet, skolemised.toStrList.toSet)) return true
    }
    false
  }

  def thetaSubsumes(t: Iterable[Clause]): Boolean = t.forall(x => this.thetaSubsumes(x))

  def getVars = {
    val vars = this.head.getVars
    for (x <- this.body) vars ++= x.getVars.filter { x => !vars.contains(x) }
    vars.toList
  }

  def toStrList: List[String] = List(head.tostring) ++ (for (x <- body) yield x.tostring)

  /**
    * Replaces all variables with a new constant symbol 'skolem0', 'skolem1' etc. Same variables correspond to the
    * same constant symbol. Constants remain intact, i.e. they are used as skolem constants themselves. Example:
    *
    * a(X,Y,Z) :-
    * p(x,q(Y,const1,2),Z),
    * not r(A,B,C).
    *
    * is turned into:
    *
    * a(skolem0,skolem1,skolem2) :-
    * p(skolem0,q(skolem1,const1,2),skolem2),
    * not r(skolem3,skolem4,skolem5).
    *
    * Returns the skolemised clause and the 'vars -> skolems' map
    *
    */

  def skolemise: (Clause, Map[String, String]) = {
    val l = this.toLiteralList
    val skmap = this.getSkolemConsts
    var temp = new ListBuffer[Literal]
    for (x <- l) {
      val m = x.skolemize(skmap).toList
      val toLit = Literal(predSymbol = x.predSymbol, terms = m, isNAF = x.isNAF)
      temp += toLit
    }
    val fl = temp.toList
    val sk = Clause(
      head = fl.head,
      body = for (x <- fl; if fl.indexOf(x) != 0) yield x)
    (sk, skmap)
  }

  /**
    * Generates skolem constants from the variables and the constants of the clause. It returns a map of the form
    * Map('X -> skolem0', 'Y -> skolem1', 'const -> const', .... ) (we use the constants as skolem constants)
    */

  private def getSkolemConsts: Map[String, String] = {
    val l = this.toLiteralList
    //print(l)
    var skolems = new ListBuffer[(String, String)]
    var counter = 0
    for (x <- l) {
      val m = x.getSkolemConsts(skolems, counter);
      skolems = m._1; counter = m._2
    }
    skolems.toMap
  }

  def toLiteralList = List(head) ++ (for (x <- body) yield x)

  def clearStatistics = {
    tps = 0
    fps = 0
    fns = 0
    seenExmplsNum = 0
    refinements = List.empty[Clause]
    previousMeanDiffCount = 0
    previousMeanScoreCount = 0
    previousMeanDiff = 0
  }

  def precision: Double = {
    val pr = tps.toFloat / (tps + fps)
    if (pr.isNaN) 0.0 else pr
  }

  def recall: Double = {
    val rec = tps.toFloat / (tps + fns)
    if (rec.isNaN) 0.0 else rec
  }

  def fscore: Double = {
    if (this.precision + this.recall == 0) 0.0
    else (2 * this.precision * this.recall) / (this.precision + this.recall)
  }

  def foilGain(funct: String) = {

    val thisCoverage = if (funct == "precision") this.precision else this.recall
    val parentCoverage = if (funct == "precision") parentClause.precision else parentClause.recall

    if (thisCoverage == 0.0) {
      // If thisCoverage == 0.0 then this rules covers nothing, it's useless, so set it's gain to 0.
      // Note that otherwise we'll have a logarithm evaluated to -infinity (log(0)).
      0.0
    } else {
      // here thisCoverage is in (0,1)
      if (parentCoverage == 1.0) { // parentCoverage == 1.0 || parentCoverage == 0.0
        // If parentCoverage == 1.0 then the parent rule is perfect, no way to beat that, so set this rule's gain to 0
        // Note that otherwise we'll have the parent's log evaluated to 0 and the gain formula
        // returning a negative value (parentTPs * log(thisCoverage), which is < 0 since thisCoverage < 1).
        // Eitherway, we only care for positive gain.
        // If, on the other hand, parentCoverage == 0.0 then thisCoverage == 0 (the parent covers nothing, so no way for
        // this rule -- a refinement --  to cover something)
        0.0
      } else {
        // here parentCoverage is in (0,1)

        val _gain = tps * (Math.log(thisCoverage) - Math.log(parentCoverage))

        // This is the correct formula, since we need the number of parent rule's tps, which are this rule's tps also.
        //val _gain = (this.parentClause.tps - tps) * (Math.log(thisCoverage) - Math.log(parentCoverage))

        // We are interested only in positive gain, therefore we consider 0 as the minimum of the gain function:
        val gain = if (_gain <= 0.0) 0.0 else _gain

        // This is the maximum gain for a given rule:
        val max = parentClause.tps.toDouble * (-Math.log(parentCoverage))
        val normalizedGain = gain / max

        if (normalizedGain > 1.0) {
          val stop = "stop"
        }

        normalizedGain
      }
    }
  }

  // newMean = (oldMean*previousCount + newDiff)/(previousCount+1)
  var previousMeanDiffCount = 0
  var previousMeanScoreCount = 0
  var previousMeanDiff = 0.0
  // This stores the previous mean score (used for pruning)
  var previousScore = 0.0

  def meanDiff(scoringFunction: String) = {

    // The - sign is to sort with decreasing order (default is with increasing)
    // Also sort clauses by length, so that sorter clauses be preferred over longer ones with the same score
    val allSorted =
      if (scoringFunction == "foilgain")
        // The parent rule should not be included here (otherwise it will always win, see the foil gain formula)
        this.refinements.sortBy { x => (-x.score(scoringFunction), -x.precision, -x.weight, x.body.length) }
      else
        (List(this) ++ this.refinements).sortBy { x => (-x.score(scoringFunction), -x.precision, -x.weight, x.body.length) }

    val bestTwo = allSorted.take(2)

    //val (best,secondBest) = (bestTwo.head,bestTwo.tail.head)
    // The correct way to do it is as the commented one above. But in some cases
    // the refinements lists is empty (this has only occurred when I use basic and auxiliary predicates in fraud).
    // This should be handled generically, a clause with no candidate refs should not be considered for specialization
    val (best, secondBest) =
      if (bestTwo.length > 1) (bestTwo.head, bestTwo.tail.head) else (bestTwo.head, bestTwo.head)
    val newDiff = best.score(scoringFunction) - secondBest.score(scoringFunction)
    val newMeanDiff = ((previousMeanDiff * previousMeanDiffCount) + newDiff) / (previousMeanDiffCount + 1)

    previousMeanDiffCount += 1
    previousMeanDiff = newMeanDiff

    (newMeanDiff, best, secondBest)
  }

  def score(scoringFunction: String): Double = {

    /*
    if (this.foilGainInit.isInfinite || this.foilGainTerm.isInfinite) {
      val debug = "stop"
    }
    */

    /*
    if (Globals.glvalues("distributed").toBoolean) {
      throw new RuntimeException("This is just to debug the distributed version, where the execution flow should not pass from here!")
    }
    */

    if (this.head.predSymbol == "initiatedAt") {
      scoringFunction match {
        case "default" => if (!precision.isNaN) precision else 0.0 // That's the standard

        //case "default" => weighted_precision

        //case "default" => if (!precision.isNaN) (tps.toFloat- (fps.toFloat - this.length.toFloat))/(tps.toFloat+fps.toFloat) else 0.0

        //case "default" => if (!precision.isNaN)  (1.0 - 1.0/(1.0+tps.toDouble)) * precision else 0.0

        case "foilgain" => foilGain("precision")
        case "fscore" => fscore
        case _ => throw new RuntimeException("Error: No scoring function given.")
      }

      //presision_length
      //compressionInit
      //foilGainInit
      //gainInt
    } else if (this.head.predSymbol == "terminatedAt") {
      scoringFunction match {
        case "default" => if (!precision.isNaN) precision else 0.0 //if (!recall.isNaN) recall else 0.0 //

        //case "default" => weighted_recall

        //case "default" => (tps.toFloat- (fns.toFloat - this.length.toFloat))/(tps.toFloat+fns.toFloat)

        //case "default" => if (!recall.isNaN) (1.0 - 1.0/(1.0+tps.toDouble)) * recall else 0.0

        case "foilgain" => foilGain("precision") //foilGain("precision")
        case "fscore" => fscore
        case _ => throw new RuntimeException("Error: No scoring function given.")
      }

      //recall_length
      //compressionTerm
      //foilGainTerm
      //gainTerm
    } else {
      // this.fscore
      /* Until now this has only been used for fraud.
       * We don't use f-score for evaluating individual
       * rules, because a rule's fns are irrelevant.
       * So we'll use precision.
       */

      //foilGainInit // No improvement!

      //gainInt      // No improvement!

      if (!precision.isNaN) precision else 0.0 // This is what I use but does not work well

      //  if (!precision.isNaN) (tps.toFloat + 10) / (tps.toFloat+10 + fps) else 0.0 // weight it just to check

      //rateDiff // No! (takes negative values)

      //tpsRelativeFrequency

      //fscore
    }
  }

  /**
    * @param rulesThatAlreadyExists is an optional paramater to avoid generating the same stuff.
    */
  def generateCandidateRefs(
      spDepth: Int,
      comparisonPredicates: List[ModeAtom],
      rulesThatAlreadyExists: Vector[Clause] = Vector.empty[Clause]): Unit = {

      /*
    * Checks if a specialization is redundant. Currently a specialization is
    * redundant if it consists only of comparison predicates of the same type.
    * For instance, this is redundant:
    *
    * blah :- close(X,Y,30,12), close(X,Y,40,12), close(X,Y,50,12)
    *
    * where close(X, Y, Z, T) means that the Euclidean distance of X and Y at time T is less than Z.
    *
    * */
      def redundant(newLits: Set[Literal]) = {
        val all = this.body ++ newLits

        val test: Set[ModeAtom] = all.map(x => x.modeAtom).toSet

        // if the test variable is a singleton then all
        // predicates are comparison predicates of the same type
        if (all.size == 1) false else test.size == 1 && comparisonPredicates.contains(test.head)
      }

    val candidateList = this.supportSet.flatMap(_.body).distinct.filter(!this.body.contains(_))

    val refinementsSets =
      (for (x <- 1 to spDepth) yield x).foldLeft(List[List[Clause]]()) { (accum, depth) =>
        val z = for (lits <- candidateList.toSet.subsets(depth).toVector if !redundant(lits)) yield Clause(this.head, this.body ++ lits)
        val z_ = LogicUtils.compressTheory(z)
        accum :+ z_
      }

    // The filtering is used by Hedge
    val flattend = refinementsSets.flatten.filter(ref => !rulesThatAlreadyExists.exists(rule => rule.thetaSubsumes(ref) && ref.thetaSubsumes(rule)))

    flattend.foreach { refinement =>
      refinement.parentClause = this
      //------------------------------------
      refinement.weight = this.weight
      //------------------------------------
      //refinement.w_pos = this.w_pos
      //------------------------------------
      refinement.supportSet = this.supportSet
      //------------------------------------
      refinement.setTypePredsLit(refinement.supportSet.head.typesPreds)
    }

    this.refinements = flattend :+ this.supportSet.head
    //this.refinements = List(this.supportSet.head)
  }

  def tostringFormal: String = this.toStrList match {
    case Nil => throw new RuntimeException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- \n" + "      " + ts.head + "."
        case _ => h + " :- \n" + (for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) s"      $x." else s"      $x,").mkString("\n")
      }
  }

  override lazy val tostring: String = this.toStrList match {
    case Nil => throw new RuntimeException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- " + ts.head + "."
        case _ => h + " :- " + (for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) s"$x." else s"$x,").mkString(" ")
      }
  }

  /* No new line after each literal */
  def tostring_debug = this.toStrList match {
    case List() => throw new RuntimeException("Cannot generate a Clause object for the empty clause")
    case h :: ts =>
      ts.length match {
        case 0 => h + "."
        case 1 => h + " :- " + ts.head + "."
        case _ => h + " :- " + (for (x <- ts) yield if (ts.indexOf(x) == ts.length - 1) s"$x." else s"$x,").mkString("")
      }
  }

  def varbed: Clause = {
    var accum = ListBuffer[Literal]()
    var map = scala.collection.mutable.Map[LogicalExpression, LogicalExpression]()
    var counter = 0
    for (x <- this.toLiteralList) {
      val (a, _, c, d) = x.variabilize(
        List(Literal(predSymbol = x.predSymbol, isNAF = x.isNAF)),
        x.terms zip x.modeAtom.args, map, List(), counter)
      val aa = Literal(a.head.predSymbol, a.head.terms, a.head.isNAF, x.modeAtom, a.head.typePreds)
      accum ++= List(aa)
      map ++ c
      counter = d
    }
    val l = accum.toList
    val out = Clause(head = l.head, body = l.tail)
    out
  }

  var typesPreds = List.empty[Literal]

  def setTypePredsStr(types: List[String]) = {
    val toLits = types.asInstanceOf[List[String]].map(Literal.parse(_))
    typesPreds = toLits
  }

  def setTypePredsLit(types: List[Literal]) = {
    typesPreds = types
  }

  def withTypePreds(modes: List[ModeAtom], extraTypePreds: List[String] = List()): Clause = {
    var types = (for (x <- this.toLiteralList)
      yield x.getTypePredicates(modes)).filter { z => z != Nil }.
      flatten.++(extraTypePreds).distinct.
      map { y => Literal.parse(y) }

    if (types.isEmpty) types = supportSet.head.typesPreds // This is only used for hard-coded bottom clauses.

    Clause(head = this.head, body = this.body ::: types)
  }

}
