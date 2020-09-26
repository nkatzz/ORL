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
