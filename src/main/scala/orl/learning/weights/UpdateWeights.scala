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

    val lambda = inps.adaRegularization
    val eta = inps.adaLearnRate
    val delta = inps.adaGradDelta
    val currentSubgradient = mistakes
    rule.subGradient += currentSubgradient * currentSubgradient
    val coefficient = eta / (delta + math.sqrt(rule.subGradient))
    val value = rule.weight - coefficient * currentSubgradient
    val difference = math.abs(value) - (lambda * coefficient)

    val result = {
      if (rule.isNew) {
        val y = coefficient * (-mistakes - lambda)
        rule.isNew = false
        if (y > 0) y else 0.0 //Clause.leastWeight //0.0
      } else {
        if (difference > 0) if (value >= 0) difference else -difference
        else 0.0 // Clause.leastWeight //0.0
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
