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

package orl.learning.woledasptr

import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.datahandling.InputHandling.InputSource
import orl.learning.Learner
import orl.logic.Clause

/**
  * Created by nkatz at 6/8/20
  */
class TheoryRevisionLearner[T <: InputSource](
    inps: RunningOptions,
    trainingDataOptions: T,
    testingDataOptions: T,
    trainingDataFunction: T => Iterator[Example],
    testingDataFunction: T => Iterator[Example])
  extends Learner(inps, trainingDataOptions, testingDataOptions, trainingDataFunction, testingDataFunction) {
  /**
    * Abstract method, to be implemented by specific learners.
    *
    */
  override def process(exmpl: Example): Unit = {
    val theory = state.getTopTheory()
  }

  /**
    * Abstract method, to be implemented by specific learners.
    *
    */
  override def generateNewRules(existingTheory: List[Clause], exmpl: Example, inps: RunningOptions): List[Clause] = ???

  /**
    * Abstract method, to be implemented by specific learners.
    * It displays statistics from the learning process, performs cross-validation
    * (if testing set is provided) etc.
    *
    */
  override def wrapUp(): Unit = ???
}
