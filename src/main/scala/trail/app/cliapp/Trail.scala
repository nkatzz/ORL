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

package trail.app.cliapp

import com.typesafe.scalalogging.LazyLogging
import trail.app.LearnRevise
import trail.app.runutils.{Argument, CMDArgs, Task}

object Trail extends LazyLogging {

  def main(args: Array[String]) = {
    System.setProperty("logback.configurationFile", "src/main/resources/logback.xml")
    val inps = CMDArgs.parseInputOptions(args)
    val task = inps.task
    if (task == CMDArgs.learnrev) {
      LearnRevise.main(args)
    } /*else if (task == CMDArgs.infer) {
      val mode = inps.mode
      if (mode == "eval") {
        ???
      } else if (mode == "show") {
        val
      } else { // then mode == 'explain'

      }
    } else {
      ???
    }*/
    /*if (inps.task.isEmpty) {
      showGeneralInfo()
      System.exit(0)
    }*/
  }

  def showOptions(args: Vector[Argument], task: String = "") = {
    val opts = args.map { arg =>
      if (task.isEmpty) {
        s"    ${arg.name}=${arg.valueType} | default=${arg.default} | applies: [${arg.relatedTo.mkString(", ")}]\n        ${arg.text}"
      } else {
        s"    ${arg.name}=${arg.valueType} | default=${arg.default}\n        ${arg.text}"
      }

    }
    opts.mkString("\n")
  }

  def showGeneralInfo() = {
      def showTasks = {
        val space = CMDArgs.tasks.keys.map(x => x.length).max + 2
        CMDArgs.tasks.values.map(x => s"    ${x.name}:${" " * (space - x.name.length)}${x.description}").mkString("\n")
      }

    val msg = s"\nUsage:\n    trail task [options]\nTasks:\n$showTasks\nOptions:\n${showOptions(CMDArgs.arguments)}"
    logger.info(msg)
  }

  def showTaskInfo(task: Task) = {
    val relevantOptions = CMDArgs.arguments.filter(x => x.relatedTo.contains(task.name))
    val msg = s"\nUsage:\n    trail ${task.name} [options]\n    ${task.description}\nOptions:\n${showOptions(relevantOptions, task.name)}"
    logger.info(msg)
  }

  def formatDescription(s: String, length: Int) = s.split(s"(?<=\\G.{$length})").map(x => s"    $x").mkString("\n")

}
