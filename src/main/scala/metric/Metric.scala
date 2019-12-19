/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package metric

import lomrf.logic.AtomicFormula
import lomrf.mln.model.Evidence

/**
  * A metric for atomic formulas is defined by a distance function over atoms
  * and a distance function over sequences of atoms.
  */
trait Metric[A <: AtomicFormula] {

  /**
    * @return the absolute normalized distance
    */
  protected def distance(x: Double, y: Double): Double =
    if (x == 0 && y == 0) 0
    else math.abs(x - y) / (x + y)

  /**
    * Distance for atoms. The function may obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  def distance(xAtom: A, yAtom: A): Double

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double

  /**
    * Append evidence information to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit evidence information (data driven).
    *
    * @param evidence an evidence database
    * @return an updated metric
    */
  def ++(evidence: Evidence): Metric[A] = this

  /**
    * Append information from atom sequences to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit atom sequences (data driven).
    *
    * @param atomSeqSeq a sequence of atom sequences.
    * @return an updated metric
    */
  def ++(atomSeqSeq: Seq[Seq[AtomicFormula]]): Metric[A] = this
}

/**
  * A structure metric for atomic formulas is defined by a distance function over
  * atoms and a distance function over sequences of atoms by specifying a matcher.
  *
  * @note It should be extended by metrics that compare the structure of
  *       the given atomic formulas.
  *
  * @tparam A the type of atomic formula
  */
trait StructureMetric[A <: AtomicFormula] extends Metric[A] {

  // Matcher used for finding a mapping between atoms sequences
  val matcher: Matcher

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  final def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double = {

    // Swap atom sequences
    val (longAtomSeq, shortAtomSeq) =
      if (xAtomSeq.length >= yAtomSeq.length) (xAtomSeq, yAtomSeq)
      else (yAtomSeq, xAtomSeq)

    // Compute the distance matrix for each pair of atoms
    val distanceMatrix = longAtomSeq.map(x => shortAtomSeq.map(distance(x, _)))

    // Compute a matching and a total cost
    val (_, unweightedDistance) = matcher(distanceMatrix)

    unweightedDistance
  }
}