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

package draftcode_tobe_moved.bioseqs.bioseqs

import java.io.File

import oled.inference.ASPSolver.solve
import oled.logic.Literal

import scala.io.Source

/**
  * Created by nkatz at 16/1/20
  */
object DataProcessing extends App {

  val interestingPath = "/home/nkatz/dev/BIO-DATA/all_symbolic_results-COMPLETE/final_symbolic_results/interesting"
  val nonInterestingPath = "/home/nkatz/dev/BIO-DATA/all_symbolic_results-COMPLETE/final_symbolic_results/not_interesting"

  val interestingASP = "/home/nkatz/dev/BIO-DATA/all-latest/symbols/asp/interesting"
  val notInterestingASP = "/home/nkatz/dev/BIO-DATA/all-latest/symbols/asp/not_interesting"

  //val BKPath = "/home/nkatz/dev/BioSequencesLearning/bk"
  val BKPath = "/home/nkatz/dev/BioSequencesLearning/bk-time-elapsed"

  class Container(val alive: String, val necrotic: String, val apoptotic: String)

  def sameState(state: Map[String, String], vals: Container) = {
    vals.alive == state("alive") && vals.necrotic == state("necrotic") && vals.apoptotic == state("apoptotic")
  }

  def updateState(vals: Container) = {
    val newState = Map("alive" -> vals.alive, "necrotic" -> vals.necrotic, "apoptotic" -> vals.apoptotic)
    newState
  }

  var counter = 1

  def process(path: String, what: String) = {

    val d = new File(path)
    val files = d.listFiles()

    for (f <- files) {
      println(s"Processing file ${f.getName}")

      var state = Map("alive" -> "", "necrotic" -> "", "apoptotic" -> "")

      val source = Source.fromFile(f)
      val lines = source.getLines()
      lines.next() // Ignore the header

      val contentASP = lines.flatMap { line =>
        val split = line.split(",")
        val time = split(0)
        val aliveVal = split(1)
        val necroticVal = split(2)
        val apoptoticVal = split(3)
        val vals = new Container(aliveVal, necroticVal, apoptoticVal)

        val aliveAtom = s"obs(alive,$aliveVal,$time)"
        val necroticAtom = s"obs(necrotic,$necroticVal,$time)"
        val apoptoticAtom = s"obs(apoptotic,$apoptoticVal,$time)"
        List(aliveAtom, necroticAtom, apoptoticAtom)

      }.toList
      source.close()

      val program = contentASP.map(x => x + ".").mkString("\n") + s"""#include "${BKPath}"."""
      val results = solve(program)

      val sorted = (contentASP ++ results). // This includes the obs(alive(g),6) stuf...
        //val sorted = (results).
        map(x => Literal.parse(x)).groupBy(x => x.terms.last.name.toInt).
        toSeq.sortBy(_._1).map(x => x._2)

      /*val program = contentASP.map(x => x+".").mkString("\n")+s"""#include "${BKPath}"."""
      val results = solve(program)

      //val sorted = (contentASP ++ results). // This includes the obs(alive(g),6) stuff...
      val sorted = (results).
        map(x => Literal.parse(x)).groupBy(x => x.terms.last.name.toInt).
        toSeq.sortBy(_._1).map(x => x._2)

      val repetionsFilteredOut = {
        sorted.map { x =>

          val (alive, necrotic, apoptotic) = x.foldLeft("", "", "") { (x, y) =>
            if (y.terms.head.name == "alive") {
              (y.predSymbol, x._2, x._3)
            } else if (y.terms.head.name == "necrotic") {
              (x._1, y.predSymbol, x._3)
            } else {
              (x._1, x._2, y.predSymbol)
            }
          }

          val vals = new Container(alive, necrotic, apoptotic)
          val result = if (!sameState(state, vals)) x.map(_.tostring+".").mkString("\n") else ""
          state = updateState(vals)
          result
        }
      }*/

      //val repetionsFilteredOut = contentASP.map(x => x+".")

      val repetionsFilteredOut = sorted.map(x => x.map(_.tostring + ".")).flatten

      val write = s"begin(model($counter)).\n$what.\n${repetionsFilteredOut.filter(x => x != "").mkString("\n")}\nend(model($counter)).\n"

      oled.utils.Utils.dumpToFile(write,
        "/home/nkatz/dev/ACE-ilProlog-1.2.20-linux/ACE-ilProlog-1.2.20/examples/bioseqs/bio.kb", "append")

      repetionsFilteredOut.filter(x => x != "") foreach println
      counter += 1
    }
  }

  process(interestingPath, "interesting")
  process(nonInterestingPath, "not_interesting")
  //files.foreach(x => println(x.getName))
}
