package utils

import trail.logic.{Clause, Literal}

import scala.io.Source
import scala.util.matching.Regex

object TransformData extends App {

  def matches(p: Regex, str: String) = p.pattern.matcher(str).matches

  val source = Source.fromFile("/home/nkatz/dev/trail-exmpsl/activity-rec/true-state.lp")
  val list = source.getLines.filter(line => !matches("""""".r, line) && !line.startsWith("%")).toList
  val x = list.head.split(" ").map(x => Literal.parseWPB2(x).terms.head.tostring+".").mkString(" ")
  source.close

  println(x)

}
