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

package orl.learning

import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.inference.ASPSolver
import orl.logic.{Clause, Constant, Literal}

/**
  * Created by nkatz at 5/4/20
  *
  * Utilities for performing theory revision via ASP-based techniques.
  */

object TheoryRevision {

  def getTypePredicates(rule: Clause): List[Literal] = {
    rule.getVariables.map(x => Literal.parse(s"${x._type}(${x.name})"))
  }

  def getTypePredicates(lit: Literal) = {
    lit.variables.map(x => Literal.parse(s"${x._type}(${x.name})"))
  }

  def revise(existingTheory: List[(Clause, Int)], bottomClauses: List[Clause],
      data: Example, inps: RunningOptions) = {

    val formSolution = (solution: Array[String]) => {
      val (useAtoms, exceptionAtoms) = solution.partition(_.startsWith("use"))
      val inducedRules = formInducedRules(bottomClauses, useAtoms.toSet)
      val (refinedRules, retainedRules, removedRules) = formSpecializedRules(existingTheory.map(_._1), exceptionAtoms.toSet)
      if (inps.removeRules) {
        (inducedRules.toList, refinedRules, retainedRules, removedRules)
      } else {
        (inducedRules.toList, refinedRules, retainedRules ++ removedRules, Nil)
      }
    }

    val inductionProgram = if (bottomClauses.nonEmpty) ruleInductionMetaProgram(bottomClauses) else ""
    val refinementProgram = if (existingTheory.nonEmpty) refinementMetaProgram(existingTheory, inps) else ""
    val include = s"${inps.globals.BK}"

    /*val fnsFpsMinimizeStatement = {
      s"fns(holdsAt(F,T)) :- example(holdsAt(F,T)), not holdsAt(F,T)." +
        s"\nfps(holdsAt(F,T)) :- not example(holdsAt(F,T)), holdsAt(F,T)." +
        s"\ntps(holdsAt(F,T)) :- example(holdsAt(F,T)), holdsAt(F,T)." +
        s"\n#minimize{1,F,T : fns(holdsAt(F,T))}." +
        s"\n#minimize{1,F,T : fps(holdsAt(F,T))}." +
        s"\n#maximize{1,F,T : tps(holdsAt(F,T))}."
    }*/

    val fnsFpsMinimizeStatement = {
      s"fns(holdsAt(F,T)) :- example(holdsAt(F,T)), not holdsAt(F,T), target(holdsAt(F,T))." +
        s"\nfps(holdsAt(F,T)) :- not example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T))." +
        s"\ntps(holdsAt(F,T)) :- example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T))." +
        s"\n#minimize{1,F,T : fns(holdsAt(F,T)) ; 1,F,T : fps(holdsAt(F,T))}." //+
      //s"\n#minimize{1,F,T : fps(holdsAt(F,T))}." //+
      //s"\n#maximize{1,F,T : tps(holdsAt(F,T))}."
    }

    val show = {
      if (!inps.removeRules) {
        s"#show.\n#show use/2.\n#show excpt/3."
      } else {
        s"#show.\n#show use/2.\n#show excpt/3.\n#show ruleIsUsed/1."
      }
    }

    val trMetaProgram = s"${data.toASP().mkString(" ")}\n\n$inductionProgram\n\n$refinementProgram\n\n$include\n\n$fnsFpsMinimizeStatement\n\n$show"
    val options = if (inps.findAllOpt) "--opt-mode=optN --opt-strategy=usc" else "--opt-mode=opt --opt-strategy=usc"

    if (inps.debug) {
      val msg = s"% Run as:\n% clingo $options ${inps.entryPath}/debug"
      orl.utils.Utils.dumpToFile(s"\n$msg\n\n$trMetaProgram", s"${inps.entryPath}/debug")
    }

    val result = ASPSolver.solve(trMetaProgram, options)

    if (inps.findAllOpt) {
      // Then the result from clingo will be a Vector[String] whose each element is an optimal solution (answer set).
      // The atoms in each solution are separated by "<@>".
      result.map { x =>
        val solution = x.split("<@>")
        formSolution(solution)
      }
    } else Vector(formSolution(result.toArray))
  }

  /**
    * Generates a meta-program to generalize a set of bottom clauses, in order to induce a new set of rules.
    * @param bottomClauses the set of bottom clauses to generate new rules from.
    *
    */
  def ruleInductionMetaProgram(bottomClauses: Seq[Clause]) = {

    val metaProgram = bottomClauses map { bc =>

      /**
        * for the j-th literal p(X) in the i-th clause, construct a tuple of the form (tryAtom, useAtom), where
        * tryAtom is "try(vars(X),j,i)" and useAtom is "use(j,i)" (X represents the variables that appear in the literal).
        *
        */
      val useTryAtoms = (bc.body zip (1 to bc.body.length)).map{ case (literal, literalId) =>
        val variablesTerm = s"vars(${literal.getVars.map(x => x.name).mkString(",")})"
        val tryAtom = s"try($variablesTerm, $literalId, ${bc.##})"
        val useAtom = s"use($literalId,${bc.##})"
        val typePreds = getTypePredicates(literal).map(_.tostring).mkString(",")
        (tryAtom, useAtom, literal.tostring, typePreds)
      }

      /**
        * Transform the i-th clause of the form p(X) :- q1(X),...,qn(X) to
        *
        * p(X) :- use(0,i), try(vars(X1),1,i),...,try(vars(Xn),n,i),typesOfVariables(X1,...,Xn).
        *
        * (the Xj's represent the variables that appear in the j-th literal).
        *
        */
      val useClause = s"${bc.head.tostring} :- " +
        s"use(0,${bc.##}),${useTryAtoms.map(x => x._1).mkString(",")},${bc.typeAtoms.mkString(",")}.\n"

      /**
        * for the j-th literal p(X) in the i-th clause, construct two clauses of the form:
        *
        *
        * try(vars(X),j,i) :- use(j,i),p(X),typeOfVariable(X).
        * try(vars(X),j,i) :- not use(j,i),typeOfVariable(X).
        *
        * (X represents the variables that appear in the literal)
        *
        */
      val tryClauses = useTryAtoms map { case (tryAtom, useAtom, literal, typePreds) =>
        val use = s"$tryAtom :- $useAtom,$literal,$typePreds."
        val notUse = s"$tryAtom :- not $useAtom,$typePreds."
        s"$use\n$notUse\n"
      }

      // This doesn't allow empty-bodied rules to be generated by the rule induction process:
      //val choiceRule = "{use(J,I)} :- bottomClauseId(I), literalId(J).\n:- use(I,J), I!=0, not use(0,J).\n:- use(0,I), {use(J,I): J!=0} == 0."
      val choiceRule = "{use(J,I)} :- bottomClauseId(I), literalId(J).\n:- use(I,J), I!=0, not use(0,J)."
      val minimizeStatement = "#minimize{1,I,J:use(J,I)}."

      val idPredicates = {
        val bcPreds = bottomClauses.map(x => s"bottomClauseId(${x.##}).").mkString(" ")
        val literalPreds = {
          val largestBCLength = bottomClauses.minBy(x => -x.body.length).body.length
          (0 to largestBCLength).map(x => s"literalId($x).").mkString(" ")
        }
        s"$bcPreds\n$literalPreds\n"
      }

      s"$useClause\n${tryClauses.mkString("\n")}\n\n$choiceRule\n$minimizeStatement\n$idPredicates\n"
    }
    metaProgram.mkString("\n")
  }

  /**
    * Synthesize a set of rules from abduced use/2 atoms.
    * @param bottomClauses the set of bottom clauses we generalized from.
    * @param useAtoms A set of atoms representing prescription to use in order to assemble the
    *                 induced rules from the rules and literals in bottomClauses.
    *
    */
  def formInducedRules(bottomClauses: Seq[Clause], useAtoms: Set[String]) = {

    val bcsIdMap = bottomClauses map (x => x.## -> x) toMap
    val useAtomsParsed = useAtoms map (Literal.parse(_))
    val useAtomsPerBC = useAtomsParsed.groupBy(x => x.terms.tail.head.name.toInt)

    val newClauses = useAtomsPerBC map { case (bcId, literals) =>

      val bc = bcsIdMap(bcId)

      val (headAtom, bodyLiteralAtoms) = literals.foldLeft(Set.empty[Literal], Set.empty[Literal]) { (x, y) =>
        if (y.terms.head.name.toInt == 0) (x._1 + y, x._2) else (x._1, x._2 + y)
      }

      if (headAtom.isEmpty) {

        /**
          * At this point use/2 atoms corresponding to body literals have been
          * generated (since useAtoms is non-empty), so if there's no head atom something's wrong
          */
        throw new RuntimeException(s"No head use/2 atom inferred for BC: ${bc.tostring}")

      } else {
        val bodyLitsMap = ((1 to bc.body.length) zip bc.body).map(x => x._1 -> x._2).toMap

        val body = bodyLiteralAtoms.map(x => bodyLitsMap(x.terms.head.name.toInt)).toList

        val newRule = Clause(head = bc.head, body = body)
        newRule.supportSet = bottomClauses.toList.filter(bottomRule => newRule.thetaSubsumes(bottomRule))
        newRule
      }
    }
    newClauses
  }

  /**
    * Transforms a set of existing rules into a form that will allow for their specialization
    * using literals drawn from the support set.
    *
    * From the i-th rule of the form head :- body in the program the following rules a generated:
    *
    * head_i :- body_i, not exception(head_i, i).
    * exception(head_i, i) :- excpt(i,j,k), not q_ijk, types(head_i), types(q_ijk).
    *
    * where q_ijk represents the k-th body literal in the j-th support rule of the i-th theory rule.
    * and types/1 is a meta-predicates that denotes type predicates for variables, e.g. person(X), entity(Y) etc.
    *
    * @param existingTheory the collection of rules that may be specialized. It is represented as a tuple (Clause, Int)
    *                       with the integer-valued representation of the rule's weight, which is used when theory
    *                       revision is combined with MAP inference.
    * @param mapInference   if true then the transformation allows to take into account the weights of the rules
    *                       in the specialization process. In particular, the transformation then becomes:
    *
    *                       head(X) :- satisfied(head(X),i).
    *                       {satisfied(head(X),i)} :- body(X), not exception(vars(head), i).
    *                       exception(head_i, i) :- excpt(i,j,k), not q_ijk, types(head_i), types(q_ijk).
    *                       :~ satisfied(head(X),i). [w_i]
    *
    *
    */
  def refinementMetaProgram(existingTheory: List[(Clause, Int)], inps: RunningOptions, mapInference: Boolean = false) = {

      /**
        * Generates one definition for the exception atom, a rule of the form
        * exception(vars(head), i) :- excpt(i,j,k), not q_ijk.
        *
        * @param rule the specialization candidate clause.
        * @param ruleId the index of the specialization candidate in the current theory.
        * @param supportRuleId the index of the support rule in the support set.
        * @param supportLiteral a support literal that may be used for specialization.
        * @param supportLiteralId the index of the supportLiteral in the body of the corresponding support rule.
        * @param exceptionAtom the exception atom that will be used at the head of the generated exception definition.
        */
      def generateExceptionDefs(rule: Clause, ruleId: Int, supportRuleId: Int,
          supportLiteral: Literal, supportLiteralId: Int, exceptionAtom: String) = {

        s"$exceptionAtom :- excpt($ruleId,$supportRuleId,$supportLiteralId)," +
          s"not ${supportLiteral.tostring},${getTypePredicates(rule.head).map(_.tostring).mkString(",")}."
      }

    val removeRules = inps.removeRules
    val typeAtoms = (r: Clause) => r.typeAtoms.mkString(",")
    val bodyStr = (r: Clause) => r.body.map(_.tostring).mkString(",")
    //val satAtom = (r: Clause) => Literal.parse(s"satisfied(${rule.head.tostring},${rule.##})")

    /**
      * Core of the method, generate the rules
      * head :- body, not exception(vars(head), i).
      * exception(vars(head), i) :- excpt(i,j,k), not q_ijk
      */
    val excpts = (existingTheory zip (1 to existingTheory.length)) map { case ((rule, weight), ruleId) =>

      //val ruleHeadVariables = rule.head.getVars
      val exceptionAtom = s"exception(${rule.head.tostring},$ruleId)"
      val exceptionAtomNegated = s"not exception(${rule.head.tostring},$ruleId)"

      val exceptionRule = {
        if (!mapInference) {
          if (rule.body.nonEmpty) {
            if (!removeRules) {
              s"${rule.head.tostring} :- ${bodyStr(rule)},$exceptionAtomNegated,${typeAtoms(rule)}."
            } else {
              s"${rule.head.tostring} :- ruleIsUsed($ruleId),${bodyStr(rule)},$exceptionAtomNegated,${typeAtoms(rule)}."
            }
          } else {
            if (!removeRules) {
              s"${rule.head.tostring} :- $exceptionAtomNegated,${typeAtoms(rule)}."
            } else {
              s"${rule.head.tostring} :- ruleIsUsed($ruleId),$exceptionAtomNegated,${typeAtoms(rule)}."
            }
          }
        } else {
          if (rule.body.nonEmpty) {
            // with MAP inference
          } else {
            // with MAP inference
          }
        }

      }

      val exceptionDefinitions = {
        for (j <- rule.supportSet zip (1 to rule.supportSet.length); k <- j._1.body zip (1 to j._1.body.length))
          yield generateExceptionDefs(rule, ruleId, j._2, k._1, k._2, exceptionAtom)
      }
      s"$exceptionRule\n${exceptionDefinitions.mkString("\n")}"
    }

    val choiceRule = {
      if (!removeRules) {
        "{excpt(I,J,K)} :- topRuleId(I), supportRuleId(J), supportLiteralId(K)."
      } else {
        "{excpt(I,J,K)} :- topRuleId(I), supportRuleId(J), supportLiteralId(K).\n{ruleIsUsed(I)} :- topRuleId(I)."
      }
    }
    val minimizeStatement = {
      if (!removeRules) {
        "#minimize{1,I,J,K:excpt(I,J,K)}."
      } else {
        "#minimize{1,I,J,K:excpt(I,J,K)}.\n#minimize{1,I:ruleIsUsed(I)}."
      }
    }
    val varsRange = {
      val top = s"topRuleId(1..${existingTheory.length})."
      val supportRule = s"supportRuleId(1..${existingTheory.map(x => x._1.supportSet.length).max})."
      //val supportLiteral = s"supportLiteralId(1..${existingTheory.map(x => x._1.supportSet.map(_.body).length).max})."

      val supportLiteral = s"supportLiteralId(1..${existingTheory.map(x => x._1).flatMap(x => x.supportSet).map(x => x.body.length).max})."

      s"$top\n$supportRule\n$supportLiteral"
    }
    val program = s"${excpts.mkString("\n")}\n$choiceRule\n$minimizeStatement\n$varsRange"
    program
  }

  /**
    * Synthesize specializations from the abduced exception atoms.
    * @param existingTheory the set of existing rules, candidates for specialization.
    * @param abducedAtoms a set of atoms of the form excpt(i,j,k) representing the
    *                       k-th body literal of the j-th support rule of the i-th rule in existingTheory.
    *
    */
  def formSpecializedRules(existingTheory: List[Clause], abducedAtoms: Set[String]) = {

    val (exceptionAtoms, useRuleAtoms) = abducedAtoms.partition(x => x.startsWith("excpt"))
    val usefulRulesIds = useRuleAtoms.map(x => Literal.parseWPB2(x).terms.head.tostring.toInt)
    val parsedAtoms = exceptionAtoms.map(Literal.parse(_))
    val indexedRules = (1 to existingTheory.length) zip existingTheory toMap
    val (specialized, retained, removed) = indexedRules.foldLeft(List.empty[Clause], List.empty[Clause], List.empty[Clause]) { (x, y) =>
      val (ruleIndex, rule) = (y._1, y._2)
      val relevantExptAtoms = parsedAtoms.filter(_.terms.head.tostring.toInt == ruleIndex)
      if (relevantExptAtoms.isEmpty) {
        if (usefulRulesIds.contains(ruleIndex)) {
          (x._1, x._2 :+ rule, x._3)
        } else {
          (x._1, x._2, x._3 :+ rule)
        }
      } else {
        val specAtoms = relevantExptAtoms map { atom =>
          val j = atom.terms.tail.head.asInstanceOf[Constant].tostring.toInt
          val k = atom.terms.tail.tail.head.asInstanceOf[Constant].tostring.toInt
          rule.getSupportLiteral(j, k)
        }

        val newRule = Clause(head = rule.head, body = rule.body ++ specAtoms)
        newRule.supportSet = rule.supportSet.filter(x => newRule.thetaSubsumes(x))
        newRule.parentClause = rule
        newRule.weight = rule.weight

        (x._1 :+ newRule, x._2, x._3)
      }
    }

    /*val (retained, removed) = nonSpecialized.partition {

    }*/
    (specialized, retained, removed)
  }

}
