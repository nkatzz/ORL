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

package orl.learning.weights

import orl.app.runutils.RunningOptions
import orl.logic.Clause

/**
  * Created by nkatz at 2/2/20
  */

object UpdateWeights {

  def adaGradUpdate(rule: Clause, mistakes: Int, inps: RunningOptions) = {

    // Recall that for newly-generated rules the weight update is different,
    // since their weight is zero and all past gradients as zero as well, so the
    // value term is zero.
    val isNewRule = rule.weight == Clause.leastWeight && rule.subGradient == 0.0

    val lambda = inps.adaRegularization //0.001 // 0.01 default
    val eta = inps.adaLearnRate //1.0 // default
    val delta = inps.adaGradDelta //1.0
    val currentSubgradient = mistakes
    rule.subGradient += currentSubgradient * currentSubgradient
    val coefficient = eta / (delta + math.sqrt(rule.subGradient))
    val value = rule.weight - coefficient * currentSubgradient
    val difference = math.abs(value) - (lambda * coefficient)

    val result = {
      if (isNewRule) {
        val x = eta/(delta + mistakes) // the coefficient here is the sqrt of the gradient (= current mistakes)
        val y = x * (mistakes - lambda)
        if (y > 0) y else Clause.leastWeight
      } else {
        if (difference > 0) if (value >= 0) difference else -difference
        else Clause.leastWeight
      }
    }
    result
  }

  /**
    *
    * Gradients here is the number of mistakes, as in AdaGrad.
    * Step is the batch count.
    */
  def adamUpdate(rule: Clause, gradients: Int, inps: RunningOptions, step: Int) = {
    val beta1 = 0.9
    val beta2 = 0.999
    val eta = 0.01

    rule.adamGradient = beta1 * rule.adamGradient + (1 - beta1) * gradients
    rule.adamSquareSubgradient = beta2 * rule.adamSquareSubgradient + (1 - beta2) * gradients * gradients

    val m = rule.adamGradient / (1 - math.pow(beta1, step))
    val v = rule.adamSquareSubgradient / (1 - math.pow(beta2, step))

    val result = rule.weight - (eta * m) / (1e-8 + math.sqrt(v))

    result
  }

}
