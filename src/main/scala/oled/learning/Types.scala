package oled.learning

import oled.logic.Clause

/**
 * Created by nkatz at 13/12/19
 */

object Types {

  /**
   * Message types
   * */
  class FinishedBatch
  class RunSingleCore
  class Run
  class StartOver
  class LocalLearnerFinished

  /**
   * Helper types
   * */
  type InferredState =  Map[String, Boolean]
  type Theory = List[Clause]


}
