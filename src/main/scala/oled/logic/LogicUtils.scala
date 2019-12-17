package oled.logic

import scala.collection.mutable.ListBuffer

/**
 * Created by nkatz at 4/12/19
 */

object LogicUtils {

  def compressTheory(theory: Iterable[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- theory) {
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  def showTheoryWithStats(clauses: Iterable[Clause], scoreFun: String) = {
    clauses.map(x => x.showWithStats(scoreFun) ).mkString("\n")
  }

}
