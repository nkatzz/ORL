package oled.learning.weights

import oled.app.runutils.RunningOptions
import oled.logic.Clause

/**
  * Created by nkatz at 2/2/20
  */

object UpdateWeights {

  def adaGradUpdate(rule: Clause, mistakes: Int, inps: RunningOptions) = {
    val lambda = inps.adaRegularization //0.001 // 0.01 default
    val eta = inps.adaLearnRate //1.0 // default
    val delta = inps.adaGradDelta //1.0
    val currentSubgradient = mistakes
    rule.subGradient += currentSubgradient * currentSubgradient
    val coefficient = eta / (delta + math.sqrt(rule.subGradient))
    val value = rule.weight - coefficient * currentSubgradient
    val difference = math.abs(value) - (lambda * coefficient)
    val result = {
      if (difference > 0) if (value >= 0) difference else -difference
      else 0.0
    }
    result
  }

}
