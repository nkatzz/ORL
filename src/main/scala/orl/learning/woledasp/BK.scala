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

package orl.learning.woledasp

/**
  * Created by nkatz at 13/2/20
  */

object BK {

  val abductionMetaProgram =
    s"""
       |
       |fns(holdsAt(F,T)) :- example(holdsAt(F,T)), not holdsAt(F,T), target(holdsAt(F,T)).
       |fps(holdsAt(F,T)) :- not example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T)).
       |tps(holdsAt(F,T)) :- example(holdsAt(F,T)), holdsAt(F,T), target(holdsAt(F,T)).
       |
       |initiatedAt(F,T) :- initiatedAt_proxy(F,T), fluent(F), time(T).
       |terminatedAt(F,T) :- terminatedAt_proxy(F,T), fluent(F), time(T).
       |
       |% Use the initiatedAt_proxy definition to discriminate between inferred atoms
       |% (from existing initiation/termination rules) and actual abduced ("guessed") atoms.
       |{initiatedAt_proxy(F,T)} :- fluent(F), time(T).
       |{terminatedAt_proxy(F,T)} :- fluent(F), time(T).
       |
       |#minimize{1,F,T: terminatedAt_proxy(F,T)}.
       |#minimize{1,F,T: initiatedAt_proxy(F,T)}.
       |
       |#minimize{1,F,T : fns(holdsAt(F,T))}.
       |#minimize{1,F,T : fps(holdsAt(F,T))}.
       |#maximize{1,F,T : tps(holdsAt(F,T))}.
       |
       |mode(1,initiatedAt_proxy(F,T),initiatedAt(F,T)) :- fluent(F),time(T).
       |mode(2,initiatedAt_proxy(F,T),terminatedAt(F,T)) :- fluent(F),time(T).
       |mode(1,terminatedAt_proxy(F,T),initiatedAt(F,T)) :- fluent(F),time(T).
       |mode(2,terminatedAt_proxy(F,T),terminatedAt(F,T)) :- fluent(F),time(T).
       |modeCounter(1..2).
       |
       |matches(initiatedAt_proxy(F,T), initiatedAt(F,T)) :- fluent(F), time(T).
       |matches(terminatedAt_proxy(F,T), terminatedAt(F,T)) :- fluent(F), time(T).
       |
       |matchesMode(ModeCounter,Atom,Mode) :- mode(ModeCounter,Atom, Mode), abduced(Atom), matches(Atom, Mode).
       |
       |abduced(initiatedAt_proxy(F,T)) :- initiatedAt_proxy(F,T).
       |abduced(terminatedAt_proxy(F,T)) :- terminatedAt_proxy(F,T).
       |
       |#show matchesMode/3.
       |
       |""".stripMargin

  /*val ScoreAndUpdateWeightsMetaProgram: String =
    """
      |
      |% satisfied/2 atoms that appear in the rules are generated during the weighted inference task.
      |% An instance of the form e.g. satisfied(initiatedAt(F,T),RuleId) means that the rule with id RuleId
      |% is an initiation rule and it is satisfied in the inferred state.
      |
      |% True groundings for initiation rules (the rule correctly fires).
      |trueGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for initiation rules (the rule incorrectly fires).
      |falseGroundingInit(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |% True groundings for termination rules (the rule correctly fires).
      |% UPDATE (2-8-2020): It seems that the requirement for the target CE holding
      |% at the previous time point is necessary here.
      |trueGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    %example(holdsAt(F,T)),
      |    fluent(F), not example(holdsAt(F,Te)), next(T,Te).
      |
      |% False groundings for termination rules (the rule incorrectly fires).
      |falseGroundingTerm(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    %example(holdsAt(F,T)),
      |    fluent(F), example(holdsAt(F,Te)), next(T,Te).
      |
      |% For top rules, groundings inferred-as-true are instances of satisfied/2.
      |inferredTrue(F,T,RuleId) :-
      |    topRule(RuleId), fluent(F), time(T),
      |    satisfied(initiatedAt(F,T),RuleId).
      |    %predicted(holdsAt(F,Te)), next(T,Te).
      |    %example(holdsAt(F,Te)).
      |inferredTrue(F,T,RuleId) :-
      |    topRule(RuleId), fluent(F), time(T),
      |    satisfied(terminatedAt(F,T),RuleId).
      |    %not predicted(holdsAt(F,Te)), next(T,Te).
      |    %not example(holdsAt(F,Te)).
      |%*
      |inferredFalse(F,T,RuleId) :-
      |    topRule(RuleId), fluent(F), time(T),
      |    satisfied(initiatedAt(F,T),RuleId),
      |    predicted(holdsAt(F,Te)), next(T,Te),
      |    not example(holdsAt(F,Te)).
      |inferredFalse(F,T,RuleId) :-
      |    topRule(RuleId), fluent(F), time(T),
      |    satisfied(terminatedAt(F,T),RuleId),
      |    not predicted(holdsAt(F,Te)), next(T,Te),
      |    example(holdsAt(F,Te)).
      |*%
      |
      |% All true groundings.
      |trueGrounding(F,T,RuleId) :- trueGroundingInit(F,T,RuleId), fluent(F), time(T).
      |trueGrounding(F,T,RuleId) :- trueGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingInit(F,T,RuleId), fluent(F), time(T).
      |falseGrounding(F,T,RuleId) :- falseGroundingTerm(F,T,RuleId), fluent(F), time(T).
      |
      |% A specialization does not participate in the inference process, so it doesn't have satisfied/2 instances. However,
      |% we may assume that its inferred-as-true instances, had it taken part in the inference, would
      |% have been exactly the inferred-as-true instances of the parent rule, at points where the specialization fires.
      |% For instance, if p(X) :- q(X) and p(X) :- q(X),r(X) are the parent rule and the specialization respectively,
      |% then if p(1) is inferred-as-true for the parent, then we assume that it would have also been inferred as true
      |% for the specialization, provided that r(1) is true in the data (so p(1) :- q(1),r(1) is a grounding of the rule).
      |% Note that even if the specialization does take part in inference, there cannot exist more inferred instances for
      |% the specialization than the ones described above.
      |
      |inferredTrue(F,T,RuleId_1) :-
      |    specialization(RuleId_1), fluent(F), time(T),
      |    parent(RuleId_1,RuleId_2), topRule(RuleId_2),
      |    inferredTrue(F,T,RuleId_2),
      |    fires(F,T,RuleId_1).
      |
      |% Helper definitions abstracting initiation/termination.
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(initiatedAt(F,T),RuleId).
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(terminatedAt(F,T),RuleId).
      |
      |ruleId(RuleId) :- topRule(RuleId).
      |ruleId(RuleId) :- specialization(RuleId).
      |ruleId(RuleId) :- emptyBodied(RuleId).
      |
      |% Example coverage counts.
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, TrueInferredAsTrue, FalseInferredAsTrue) :-
      |    ruleId(RuleId),
      |    ActualTrueGroundings = #count {F,T: trueGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualFalseGroundings = #count {F,T: falseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    TrueInferredAsTrue = #count {F,T: trueGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)},
      |    FalseInferredAsTrue = #count {F,T: falseGrounding(F,T,RuleId), inferredTrue(F,T,RuleId), fluent(F), time(T)}.
      |
      |% No fluent/1 predicate here, we need the inferred ones, not all groundings
      |inertia(holdsAt(F,T)) :- holdsAt(F,T), endTime(T).
      |
      |total_groundings(X) :- X = #count{ holdsAt(F,T): fluent(F), time(T) }.
      |
      |#show.
      |#show resultTopRule/5.
      |#show resultSpecialization/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |""".stripMargin*/

  val ScoreAndUpdateWeightsMetaProgram: String =
    """
      |truePositiveGrounding(F,T,RuleId) :-
      |    satisfied(initiatedAt(F,T),RuleId),
      |    %not predicted(holdsAt(F,T)),
      |    example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |truePositiveGrounding(F,T,RuleId) :-
      |    satisfied(terminatedAt(F,T),RuleId),
      |    %predicted(holdsAt(F,T)),
      |    not example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |falsePositiveGrounding(F,T,RuleId) :-
      |    satisfied(initiatedAt(F,T),RuleId),
      |    %predicted(holdsAt(F,T)),
      |    not example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |falsePositiveGrounding(F,T,RuleId) :-
      |    satisfied(terminatedAt(F,T),RuleId),
      |    %not predicted(holdsAt(F,Te)),
      |    example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |falseNegativeGrounding(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    not satisfied(initiatedAt(F,T),RuleId),
      |    example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |falseNegativeGrounding(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    not satisfied(terminatedAt(F,T),RuleId),
      |    %example(holdsAt(F,T)),
      |    not example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |actuallyFalseGrounding(F,T,RuleId) :-
      |    fires(initiatedAt(F,T),RuleId),
      |    not example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |actuallyFalseGrounding(F,T,RuleId) :-
      |    fires(terminatedAt(F,T),RuleId),
      |    example(holdsAt(F,Te)),
      |    next(T,Te), fluent(F).
      |
      |% Helper definitions abstracting initiation/termination.
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(initiatedAt(F,T),RuleId).
      |fires(F,T,RuleId) :- fluent(F), time(T), fires(terminatedAt(F,T),RuleId).
      |
      |ruleId(RuleId) :- topRule(RuleId).
      |ruleId(RuleId) :- specialization(RuleId).
      |ruleId(RuleId) :- emptyBodied(RuleId).
      |
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrue, InferredFalse) :-
      |    topRule(RuleId), not emptyBodied(RuleId),
      |    InferredTrue = #count {F,T: truePositiveGrounding(F,T,RuleId), fluent(F), time(T)},
      |    NotInferredTrue = #count {F,T: falseNegativeGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualTrueGroundings = InferredTrue + NotInferredTrue,
      |    ActualFalseGroundings = #count {F,T: actuallyFalseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    InferredFalse = #count {F,T: falsePositiveGrounding(F,T,RuleId), fluent(F), time(T)}.
      |
      |resultTopRule(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrue, InferredFalse) :-
      |    emptyBodied(RuleId),
      |    InferredTrue = #count {F,T: truePositiveGrounding(F,T,RuleId), fluent(F), time(T)},
      |    NotInferredTrue = #count {F,T: falseNegativeGrounding(F,T,RuleId), fluent(F), time(T)},
      |    ActualTrueGroundings = InferredTrue + NotInferredTrue,
      |    ActualFalseGroundings = #count {F,T: actuallyFalseGrounding(F,T,RuleId), fluent(F), time(T)},
      |    InferredFalse = #count {F,T: falsePositiveGrounding(F,T,RuleId), fluent(F), time(T)}.
      |
      |% A specialization does not participate in the inference process, so it doesn't have satisfied/2 instances. However,
      |% we may assume that its inferred-as-true instances, had it taken part in the inference, would
      |% have been exactly the inferred-as-true instances of the parent rule, at points where the specialization fires.
      |% For instance, if p(X) :- q(X) and p(X) :- q(X),r(X) are the parent rule and the specialization respectively,
      |% then if p(1) is inferred-as-true for the parent, then we assume that it would have also been inferred as true
      |% for the specialization, provided that r(1) is true in the data (so p(1) :- q(1),r(1) is a grounding of the rule).
      |% Note that even if the specialization does take part in inference, there cannot exist more inferred instances for
      |% the specialization than the ones described above.
      |
      |resultSpecialization(RuleId, ActualTrueGroundings, ActualFalseGroundings, InferredTrue, InferredFalse) :-
      |    specialization(RuleId), parent(RuleId,ParentId), topRule(ParentId),
      |    InferredTrue = #count {F,T: truePositiveGrounding(F,T,ParentId), fires(F,T,RuleId), fluent(F), time(T)},
      |    NotInferredTrue = #count {F,T: falseNegativeGrounding(F,T,ParentId), fires(F,T,RuleId), fluent(F), time(T)},
      |    ActualTrueGroundings = InferredTrue + NotInferredTrue,
      |    ActualFalseGroundings = #count {F,T: actuallyFalseGrounding(F,T,ParentId), fires(F,T,RuleId), fluent(F), time(T)},
      |    InferredFalse = #count {F,T: falsePositiveGrounding(F,T,ParentId), fires(F,T,RuleId), fluent(F), time(T)}.
      |
      |% No fluent/1 predicate here, we need the inferred ones, not all groundings
      |inertia(holdsAt(F,T)) :- holdsAt(F,T), endTime(T).
      |
      |total_groundings(X) :- X = #count{ holdsAt(F,T): fluent(F), time(T) }.
      |
      |#show.
      |#show resultTopRule/5.
      |#show resultSpecialization/5.
      |#show total_groundings/1.
      |#show inertia/1.
      |
      |""".stripMargin

}
