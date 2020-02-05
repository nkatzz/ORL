package oled.learning

import com.typesafe.scalalogging.LazyLogging
import oled.app.runutils.RunningOptions
import oled.logic.Clause

/**
  * Created by nkatz at 31/1/20
  */

class PruningSpecs(val minPrecision: Double, val bodyLength: Int, val oldness: Int) {}

class Pruning(specs: PruningSpecs, inps: RunningOptions) extends LazyLogging {

  private val weightLearning = inps.weightLean
  private val acceptablePrecision = specs.minPrecision
  private val maxBodyLength = specs.bodyLength
  private val oldness = specs.oldness

  /**
    * A rule is hopeless if its precision
    * */
  def isHopeless(c: Clause) = {

  }

  def showPruned(c: Clause) = {
    // Note that the number of examples a rule has been evaluated on is the number of examples
    // it fires on, NOT the number of examples seen so far in the stream. Therefore, we're pruning
    // if the rule is of low quality after TPs+FPs examples.
    val msg =
    s"\n===========================================================\n" +
      s"\nPruned clause (Precision: ${c.precision} | TPs: ${c.tps} FPs: ${c.fps} FNs: ${c.fns} | Weight: ${c.weight})\n\n${c.tostring}\n\n" +
      s"After ${c.seenExmplsNum} examples." +
      s"\n===========================================================\n"
    logger.info(msg)
  }

}
