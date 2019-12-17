package oled.logic

import oled.app.runutils.Globals
import oled.logic.parsers.{ClausalLogicParser, ModesParser, PB2LogicParser}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

/**
 * Created by nkatz at 4/12/19
 */

object Literal {

  val empty: Literal = Literal()


  /*
  * I'm quiting parsing based on parser combinators, I'll the parboiled lib which is much faster.
  * ATTENTION: The PB2LogicParser cannot currently handle whitespace in the input strings (I need to fix it). This
  * doesn't happen in the app right now but it might be a problem in the future. If any issue appears just comment-out
  * this version of the parse method and use the one above, based on parser combinators.
  * */
  def parse(lit: String, mode: String = "") = parseWPB2(lit, mode)

  /* As above, using the Parboiled2 parser (faster). */
  def parseWPB2(lit: String, mode: String = "") = {

    def getModeAtom(atom: String): ModeAtom = {
      val p = new ModesParser
      p.getParseResult(p.parseModes(p.mode, atom))
    }

    mode match {
      case "" => PB2LogicParser.parseAtom(lit).asInstanceOf[Literal]
      case _ =>
        val l = PB2LogicParser.parseAtom(lit).asInstanceOf[Literal]
        val m = getModeAtom(mode)
        Literal(predSymbol = l.predSymbol, terms = l.terms, isNAF = l.isNAF, modeAtom = m)
    }
  }

  def toLiteral1(lit: String, mode: ModeAtom = ModeAtom("", List())): Literal = {
    val p = new ClausalLogicParser
    val l = p.getParseResult(p.parse(p.literal, lit)).asInstanceOf[Literal]
    val out = Literal(predSymbol = l.predSymbol, terms = l.terms, isNAF = l.isNAF, modeAtom = mode)
    out
  }

  def toLiteral2(lit: Literal, mode: ModeAtom = ModeAtom("", List())): Literal = {
    val out = mode match {
      case ModeAtom("", List(), false) => lit
      case _ => Literal(predSymbol = lit.predSymbol, terms = lit.terms, isNAF = lit.isNAF, modeAtom = mode)
    }
    out
  }


  def toMLNClauseLiteral(l: Literal) = {

    def handleInner(l: Literal): List[LogicalExpression] = {
      val inner = l.terms.foldLeft(List[LogicalExpression]()) { (stack, currentTerm) =>
        currentTerm match {
          case x: Constant => stack :+ Constant(x.name.capitalize)
          case x: Variable => stack :+ Constant(x.name.take(1).toLowerCase() + x.name.drop(1))
          case x: Literal =>
            val z = handleInner(x)
            stack :+ Literal(predSymbol = x.predSymbol, terms = z, isNAF = x.isNAF)
        }
      }
      inner
    }
    val toMLN = handleInner(l)
    Literal(l.predSymbol.capitalize, toMLN, isNAF = l.isNAF)
  }


  def types(l: String, mode: ModeAtom, globals: Globals) = {
    val modeDeclarations = globals.MODEHS ++ globals.MODEBS
    def terms(lit: Literal): List[LogicalExpression] = {
      val (in, out, grnd) = lit.placeMarkers
      val v = in ++ out ++ grnd
      v match {
        case Nil =>
          val mode = lit.matchingMode(modeDeclarations)
          if (!mode.isEmpty) {
            val l = Literal(predSymbol = lit.predSymbol, terms = lit.terms,
              isNAF = true, modeAtom = mode, typePreds = lit.typePreds)
            l.variables(modeDeclarations)
          } else { Nil }
        case _ => v
      }
    }
    val lit = toLiteral1(l,mode)
    val termTypes = terms(lit) map {x => s"${x._type}(${x.tostring})"}
    termTypes.distinct.mkString(",")
  }

  /* Converts a ground literal in ASP format into MLN format.
   *
   * E.g. initiatedAt(meeting(id1, id2),2000) --> InitiatedAt(Meeting_id1_id2,2000)
   *
   * This does not work for variabilized literals (throws an exception).
   * Variabilized literals are only used (at the MLN side) in rules.
   * */
  def toMLNFlat(l: Literal) = {

    def formFlatConstTerm(funct: String, args: List[LogicalExpression]) = {
      val t = s"${funct.capitalize}_${args.map(z => z.name.capitalize).mkString("_")}"
      Constant(t)
    }

    def flatten(l: Literal): List[Constant] = {
      val flattenInner = l.terms.foldLeft(List[Constant]()) { (stack, currentTerm) =>
        currentTerm match {
          case x: Constant => stack :+ Constant(x.name.capitalize)
          // No variables, this only applies to ground clauses.
          case x: Variable => throw new RuntimeException(s"Found variable: $x while transforming ${l.tostring} to MLN flattened form.")
          case x: Literal =>
            if (x.terms.forall(_.isConstant)) {
              stack :+ formFlatConstTerm(x.predSymbol, x.terms)
            } else {
              val f = flatten(x)
              stack :+ formFlatConstTerm(x.predSymbol, f)
            }
        }
      }
      flattenInner
    }

    val flat = flatten(l)
    Literal(l.predSymbol.capitalize, flat, isNAF = l.isNAF)
  }

}

/**
 * A literal is a compound term of the form p(x1,...xn), possibly preceded with 'not' ( 'not p(x1,...xn)' ),
 * in which case it is a negated literal. 'p' is the functor of the literal and xi's are its terms. Each xi
 *  is either a variable, a constant or a non-negated literal.
 *
 * @param predSymbol the predicate/function symbol of the literal.
 * @param terms the inner terms of the literal. This is a var so that it can be updated, by populating the term objects
 * by indicators on whether they correspond to input-output vars or constants, a process that takes place during the
 * construction of the Literal object, by extracting relevant information from the accompanying modeAtom (if one is present
 * with the input). I don't know if this is the best way to do it (having vars), but its seems messy to create a companion object
 * for a case class (as this one).
 * @param isNAF true or false depending on whether the literal is negated or not.
 * @param modeAtom (optional) mode declaration pattern. This is the pattern according to which the literal has been generated.
 * The mode declaration is used  to annotate the variables and constants of the
 * literal with additional information (types/sorts of constants/variables, input or output variables), which is used in the
 * process of variabilizing the clause in which this literal belongs.
 * @param typePreds an (optional) list of typing predicates, extracted from a matching mode declaration,
 *  for the literal's variables and constants.
 *
 */
case class Literal(predSymbol: String = "",
                   terms: List[LogicalExpression] = Nil,
                   isNAF: Boolean = false,
                   modeAtom: ModeAtom = ModeAtom("", Nil),
                   typePreds: List[String] = Nil) extends LogicalExpression {

  var mlnTruthValue: Boolean = false

  lazy val arity: Int = terms.length

  lazy val negated: Literal = Literal(predSymbol = this.predSymbol, terms = this.terms, isNAF = true, modeAtom = this.modeAtom, typePreds = this.typePreds)
  lazy val nonNegated: Literal = Literal(predSymbol = this.predSymbol, terms = this.terms, isNAF = false, modeAtom = this.modeAtom, typePreds = this.typePreds)

  /**
   * Returns this literal's mode placemarkers.
   * */
  lazy val placeMarkers: (List[LogicalExpression], List[LogicalExpression], List[LogicalExpression]) = getPlmrkTerms(Nil, Nil, Nil, this.terms zip this.modeAtom.args)

  /**
   * Returns a negated version of this literal
   * */
  def negateThis = {
    if (this.isNAF) Literal(predSymbol = this.predSymbol, terms = this.terms, modeAtom = this.modeAtom, typePreds = this.typePreds)
    else Literal(predSymbol = this.predSymbol, terms = this.terms, isNAF = true, modeAtom = this.modeAtom, typePreds = this.typePreds)
  }

  /**
   * Conerts this literal to MLN syntax.
   * */
  def toMLN: Literal = Literal(predSymbol = this.predSymbol.capitalize, terms = this.termsToMLN, isNAF = this.isNAF)

  /**
   * @param map  a map of expressions
   * @return a Literal that results by replacing x with y in the current literal, for each x -> y found in map.
   */
  def replaceAll(map: Map[_ <: LogicalExpression, _ <: LogicalExpression]): Literal = map.foldLeft(this)((x, y) => x.replace(y._1, y._2))

  /**
   * @return a string representation of this.
   * */
  override val tostring: String = terms match {
    case List() => predSymbol
    case _ =>
      val prefix = if (isNAF) s"not $predSymbol" else predSymbol
      prefix + "(" + (for (
        a <- terms; x = a match {
          case x @ (_: Constant | _: Variable | _: Literal ) => x
          case _ => throw new RuntimeException(s"Unexpected type of inner term while parsing Literal: $this")
        }
      ) yield x.tostring).mkString(",") + ")"
  }

  override def tostringQuote: String = terms match {
    case List() => predSymbol
    case _ =>
      val prefix = if (isNAF) s"not $predSymbol" else predSymbol;
      prefix + "(" + (for (
        a <- terms; x = a match {
          case x @ (_: Constant | _: Variable | _: Literal ) => x
          case _ => throw new RuntimeException("Unexpected type of inner term while parsing Literal.")
        }
      ) yield x.tostringQuote).mkString(",") + ")"
  }

  /**
   * @return a string representation of this in MLN syntax.
   * */
  lazy val tostringMLN: String = terms match {
    case List() => predSymbol
    case _ =>
      val prefix = if (isNAF) s"!$predSymbol" else predSymbol
      prefix + "(" + (for (
        a <- terms; x = a match {
          case x @ (_: Constant | _: Variable | _: Literal ) => x
          case _ => throw new RuntimeException(s"Unexpected type of inner term while parsing Literal: $this")
        }
      ) yield x.tostring).mkString(",") + ")"
  }

  /**
   * @return a mode declaration atom that matches this literal.
   *         If none is found, returns the empty mode atom ( ModeAtom("",List() ).
   * */
  def matchingMode(modes: List[ModeAtom]): ModeAtom = {

    def matchesMode(remaining: List[(LogicalExpression, LogicalExpression)]): Boolean = {
      remaining match {
        case head :: tail => head match {
          case (n: Constant, m @ (_: PlmrkPos | _: PlmrkNeg | _: PlmrkConst)) => matchesMode(tail)
          case (n: Variable, m @ (_: PlmrkPos | _: PlmrkNeg))                 => matchesMode(tail)
          case (n: Variable, m: PlmrkConst) =>
            throw new RuntimeException("Found a variabilized term that corresponds to a grplmrk.")
          case (n: Literal, m: ModeAtom) =>
            if (n.predSymbol != m.predSymbol || n.arity != m.arity) false else matchesMode(n.terms zip m.args)
          case _ => throw new RuntimeException("Getting matching mode: Found unexpected term pairing.")
        }
        case Nil => true
      }
    }

    //val (modeHs, modeBs) = modes.partition(x => x.predSymbol == Globals.modeHeadSymbol)
    var out: ModeAtom = ModeAtom("", List())
    this.modeAtom match {
      case ModeAtom("", Nil, false) =>
        val loop = new Breaks;
        loop.breakable {
          for (x <- modes) {
            val test = if (this.predSymbol != x.predSymbol || this.arity != x.arity) false
            else matchesMode(this.terms zip x.args)
            if (test) {
              out = x
              loop.break()
            }
          }
        }
      case _ => this.modeAtom
    }
    out
  }

  /**
   * Variabilizes a literal. If a matching mode declaration atom is passed with the input, then the literal is variabilzed according
   * to the directives provided by that atom. Else (if no mode atom is present), each constant of the literal is replaced by a new
   * variable (TODO: this is not implemented yet, see comments below). The variabilization of a literal is part of the process of
   * the variabilization of a clause. In this process, constants of the literal that are present in other literals of the clause,
   * which have already been variabilized, should be replaced by the same variable that has already been used for these constants.
   *
   * @param previousMap a map containing previous bindings of constants to variables.
   * @param accum  an accumulator that collects competed (variabilized) compound sub-terms.
   * @param remaining  a list containing all sub-terms remaining to be variabilized.
   * @param ttypes  a list collecting typing predicates for the generated variables,
   *  e.g. person(X1), time(X100) etc.
   * @param counter  a counter that is incremented by 1 each time a new variable is generated.
   * The name a new variable is simply "X"+currentCounterValue.
   * @param runningMode  a flag indicating a "mode" (purpose) for which this method is called. Default is
   * "", in which case the literal is simply variabilized. If mode = "extract-mode-terms", then this method
   * is called on a ground literal and it processes the corresponding mode declaration, extracting a tuple
   * (in,out,grnd) representing the terms of the ground atom that correspond to input, output or ground
   * placemarkers respectively
   */
  def variabilize(accum: List[Literal],
                  remaining: List[(LogicalExpression, LogicalExpression)],
                  previousMap: scala.collection.mutable.Map[LogicalExpression, LogicalExpression],
                  ttypes: List[String],
                  counter: Int,
                  runningMode: String = ""): (List[Literal], List[String], scala.collection.mutable.Map[LogicalExpression, LogicalExpression], Int) = {

    // x is a tuple (x1,x2), where x1 is a literal's constant and x2 is it's type as specified by the modeAtom
    def f(x: (LogicalExpression, String), sign: String, tail: List[(LogicalExpression, LogicalExpression)],
          map: scala.collection.mutable.Map[LogicalExpression, LogicalExpression]) = {

      val cur = accum match {
        case Nil => Literal(predSymbol = this.predSymbol, terms = List(), isNAF = this.isNAF)
        case _   => accum.last
      }

      val (litUpdate, typesUpdate, varCountUpdate) = sign match {
        case "#" =>
          // a term corresponding to constant placemarker remains intact
          (Literal(predSymbol = cur.predSymbol, terms = cur.terms :+ x._1, isNAF = cur.isNAF), ttypes, counter)
        case _ =>
          // if the constant has been variabilized previousely, use the same var.
          if (map.keySet.contains(x._1)) {
            (Literal(predSymbol = cur.predSymbol, terms = cur.terms :+ map(x._1), isNAF = cur.isNAF), ttypes, counter)
          } else {
            // else, use a new one
            val newVar = Variable("X" + counter, "+", x._2)
            map += (x._1 -> newVar)
            (Literal(predSymbol = cur.predSymbol, terms = cur.terms :+ newVar, isNAF = cur.isNAF),
              ttypes :+ x._2 + "(X" + counter + ")", counter + 1)
          }
      }
      this.variabilize(accum.tail :+ litUpdate, tail, map, typesUpdate, varCountUpdate)
    }
    remaining match {
      case head :: tail => head match {
        case (x: Constant, y: PlmrkPos)   => f((x, y._type), "+", tail, previousMap)
        case (x: Constant, y: PlmrkNeg)   => f((x, y._type), "-", tail, previousMap)
        case (x: Constant, y: PlmrkConst) => f((x, y._type), "#", tail, previousMap)
        case (x: Literal, y: ModeAtom) =>
          val (varbed, newTypes, newMap, newCount) =
            this.variabilize(List(Literal(x.predSymbol)), x.terms zip y.args, previousMap, List(), counter)
          val pop = accum.last
          this.variabilize(List(Literal(predSymbol = pop.predSymbol, terms = pop.terms ::: varbed, isNAF = pop.isNAF)),
            tail, newMap, ttypes ::: newTypes, newCount)
        case _ => throw new RuntimeException("Variabilizing Literal " + this.tostring + ": Found unexpected type")
      }
      case Nil =>
        val pop = accum.last
        (accum.tail :+
          Literal(predSymbol = pop.predSymbol, terms = pop.terms, isNAF = pop.isNAF), ttypes, previousMap, counter)
    }
  }

  /**
   * Extracts the terms of the literal marked as input-output or ground terms.
   *
   * @param in  an accumulator for input terms
   * @param out  an accumulator for output terms
   * @param grnd  an accumulator for ground terms
   * @param remaining  the (zipped) terms of the literal and the mode atom
   * that remain to be checked
   * @return a tuple (in,out,ground) carrying the marked terms
   */
  def getPlmrkTerms(in: List[LogicalExpression], out: List[LogicalExpression], grnd: List[LogicalExpression],
                    remaining: List[(LogicalExpression, LogicalExpression)]): (List[LogicalExpression], List[LogicalExpression], List[LogicalExpression]) = {
    remaining match {
      case head :: tail => head match {
        case (x: Constant, y: PlmrkPos) => getPlmrkTerms(in ::: List(Constant(x.name, "+", y._type)), out, grnd, tail)
        case (x: Constant, y: PlmrkNeg) => getPlmrkTerms(in, out ::: List(Constant(x.name, "-", y._type)), grnd, tail)
        case (x: Constant, y: PlmrkConst) => getPlmrkTerms(in, out, grnd ::: List(Constant(x.name, "#", y._type)), tail)
        case (x: Variable, y: PlmrkPos) => getPlmrkTerms(in ::: List(Variable(x.name, "+", y._type)), out, grnd, tail)
        case (x: Variable, y: PlmrkNeg) => getPlmrkTerms(in, out ::: List(Variable(x.name, "-", y._type)), grnd, tail)
        case (x: Variable, y: PlmrkConst) => getPlmrkTerms(in, out, grnd ::: List(Variable(x.name, "#", y._type)), tail)
        case (x: Literal, y: ModeAtom) =>
          val (newin, newout, newconst) = getPlmrkTerms(in, out, grnd, x.terms zip y.args)
          getPlmrkTerms(newin, newout, newconst, tail)
        case _ => throw new RuntimeException(this.tostring + ": Unexpected type.")
      }
      case Nil => (in, out, grnd)
    }
  }

  /**
   * Converts the inner terms of the literal to MLN syntax.
   * */
  private def termsToMLN: List[LogicalExpression] = {
    this.terms map {
      // variables are always of the form X0, X1, X2 etc, so turning them to lower case
      // simply converts the X, no other changes.
      case y: Variable => Constant(y.name.toLowerCase)
      case y: Constant => Constant(y.name.capitalize)
      case y: Literal =>
        val l = y
        val m = l.termsToMLN
        Literal(predSymbol = l.predSymbol, terms = m, isNAF = l.isNAF)
    }
  }

  /**
   * Skolemize this literal (replace all variables with skolem constants).
   * */
  def skolemize(skolems: Map[String, String], accum: ListBuffer[LogicalExpression] = ListBuffer[LogicalExpression]()): ListBuffer[LogicalExpression] = {
    var temp = new ListBuffer[LogicalExpression]
    def keyExists = (x: Any) => if (skolems.keySet.exists(_ == x)) true else false
    def append = (x: LogicalExpression) => temp += x
    for (x <- this.terms) x match {

      case y: Variable =>
        val name = y.name
        if (keyExists(name)) append(Constant(skolems(name)))
        else throw new RuntimeException("Skolemise: Found a variable without corresponding skolem constant.")

      case y: Constant =>
        val name = y.name
        if (keyExists(name)) append(Constant(skolems(name)))
        else throw new RuntimeException("Skolemise: Found a constant without corresponding skolem constant.")

      case y: Literal =>
        val l = y
        val m = l.skolemize(skolems, temp)
        val toLit = Literal(predSymbol = l.predSymbol, terms = m.toList, isNAF = l.isNAF)
        temp += toLit

      case _ => throw new RuntimeException("Skolemise: Unexpected type.")
    }
    temp
  }

  /**
   * @return this literal's skolem constants.
   * */
  def getSkolemConsts(skolems: ListBuffer[(String, String)], counter: Int): (ListBuffer[(String, String)], Int) = {
    var c = counter; var s = skolems
    def f = (x: String, y: String) => if (!s.contains(x)) s += x -> y else s
    def g = (x: Int) => c += x
    for (x <- this.terms) x match {
      case y: Variable =>
        f(y.name, "skolem" + c); g(1)
      case y: Constant => f(y.name, y.name) // use the constant as a skolem constant
      case y: Literal =>
        val m = y.getSkolemConsts(s, c)
        s = m._1; c = m._2
      case _ => throw new RuntimeException("Skolemize: Unexpected type of inner term.")
    }
    (s, c)
  }

  /**
   * Replace all occurrences of thisExpr in this literal with thatExpr
   * @param thisExpr
   * @param thatExpr
   * @return a Literal with all occurrences of thisExpr replaced by thatExpr
   **/
  def replace(thisExpr: LogicalExpression, thatExpr: LogicalExpression): Literal = {
    var temp = new ListBuffer[LogicalExpression]
    def append = (x: LogicalExpression) => if (x == thisExpr) temp += thatExpr else temp += x
    for (x <- this.terms) x match {
      case y: Variable => append(y)
      case y: Constant => append(y)
      case y: Literal =>
        if (y == thisExpr) temp += thatExpr
        else {
          val l = y
          val m = l.replace(thisExpr, thatExpr)
          temp += m
        }
      case _ => throw new RuntimeException("Replace, don't know what to do.")
    }
    Literal(predSymbol = this.predSymbol, terms = temp.toList, isNAF = this.isNAF)
  }

  /**
   * Get all variables from this Literal
   */
  def getVars: ListBuffer[Variable] = {
    val vars = new ListBuffer[Variable]
    for (x <- this.terms) x match {
      case y: Variable => if (!vars.contains(y)) vars += y
      case y: Literal =>
        val z = y.getVars
        vars ++= z.toList.filter { v => !vars.contains(v) }
      case _ =>
    }
    vars
  }

  /**
   * Returns a list of typing predicates for the variables of the literal.
   * e.g. 'time(X)' if X is of type 'time'
   */

  def getTypePredicates(modes: List[ModeAtom]): List[String] = {
    val f = (x: LogicalExpression) => x.asInstanceOf[Variable]
    val vars = this.variables(modes)
    val tpreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")"
    tpreds
  }

  def getConstantsTypes(modes: List[ModeAtom]): List[String] = {
    val f = (x: LogicalExpression) => x.asInstanceOf[Constant]
    val vars = this.constants(modes)
    val tpreds = for (x <- vars) yield f(x)._type + "(" + f(x).name + ")"
    tpreds
  }

  /**
   * @return a list of this literal's variables (helper method).
   * */
  def variables(modes: List[ModeAtom]): List[LogicalExpression] = {
    val (in, out, _) = this.placeMarkers
    val v = in ++ out
    v match {
      case Nil =>
        val mode = this.matchingMode(modes)
        if (!mode.isEmpty) {
          val l = Literal(predSymbol = this.predSymbol, terms = this.terms, isNAF = true, modeAtom = mode, typePreds = this.typePreds)
          l.variables(modes)
        } else Nil
      case _ => v filter (x => !x.isConstant)
    }
  }

  /**
   * @return a list of this literal's constants (helper method).
   * */
  def constants(modes: List[ModeAtom]): List[LogicalExpression] = {
    val (in, out, grnd) = this.placeMarkers
    val v = in ++ out ++ grnd
    v match {
      case Nil =>
        val mode = this.matchingMode(modes)
        if (!mode.isEmpty) {
          val l = Literal(predSymbol = this.predSymbol, terms = this.terms, isNAF = true, modeAtom = mode, typePreds = this.typePreds)
          l.constants(modes)
        } else { Nil }

      case _ => v filter (x => !x.isVariabe)
    }
  }

  /**
   * Get all terms of this Literal that correspond to variables.
   */

  def getTermsThatCorrespondToVars(modes: List[ModeAtom]): List[_ <: LogicalExpression] = {
    val mode = this.matchingMode(modes)
    getTermsThatCorrespondToVars(mode)
  }

  def getTermsThatCorrespondToVars(mode: ModeAtom): List[_ <: LogicalExpression] = {
    val out = new ListBuffer[T forSome { type T <: LogicalExpression }]
    for (x <- this.terms zip mode.args) {
      x match {
        case (term, m @ (_: PlmrkPos | _: PlmrkNeg)) => out += term
        case (x: Literal, y: ModeAtom) =>
          val inner = x.getTermsThatCorrespondToVars(y)
          out ++= inner
        case _ =>
      }
    }
    out.toList.distinct
  }






  /**
   * @return the constant that represents a threshold value for comparison.
   *         This is used for comparison predicates only. For example,
   *         if this is close(X,Y,40,T) and the corresponding declaration is:
   *
   *         comparisonPredicate(close(+person,+person,#threshold_value,+time), lessThan, comparison_term_position(3))
   *
   *         Then this method returns 40.
   * */
  def getComparisonTerm = {
    val m = this.modeAtom
    if (m.isComparisonPredicate) {
      // Note that since m is a comparison predicate template, its comparisonTermPosition list cannot be empty
      val first = this.terms(m.comparisonTermPosition.head - 1)
      val rest = m.comparisonTermPosition.tail
      if (rest.nonEmpty) rest.foldLeft(first) ( (term, position) => term.asInstanceOf[Literal].terms(position - 1) ) else first
    } else Constant()
  }

}
