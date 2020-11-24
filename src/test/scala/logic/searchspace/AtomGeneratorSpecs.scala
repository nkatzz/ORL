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

package logic.searchspace

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import trail.learning.utils.searchspace.AtomGenerator
import trail.logic.Variable

class AtomGeneratorSpecs extends AnyFunSuite with LazyLogging {

  val domainConstants = Map("fluent" -> Array("alive", "dead", "loaded"), "event" -> Array("shoot", "load"))

  test("Testing the functionality of generating atoms from mode declaration templates") {
    val modeAtom = "initiatedAt(#fluent,arg(#fluent),arg(#event),+time,+person,arg(+time),+time)"

    val v1 = Variable("X1", "+", "time")
    val v2 = Variable("X2", "+", "person")
    val v3 = Variable("X3", "+", "time")
    val v4 = Variable("X4", "+", "time")
    val existingInputVars = List(v1, v2, v3, v4)

    val generator1 = new AtomGenerator(modeAtom, domainConstants, true)
    val generator2 = new AtomGenerator(modeAtom, domainConstants, false, existingInputVars)

    val atoms1 = generator1.generate
    val atoms2 = generator2.generate

    assert(atoms1.size == 12)
    assert(atoms2.size == 288)
  }

  test("No output variables are allowed in the heads of rules.") {
    val modeAtom = "initiatedAt(#fluent,arg(#fluent),arg(#event),+time,+person,arg(+time),-time)"
    val generator = new AtomGenerator(modeAtom, domainConstants, true)
    try {
      generator.generate
      fail()
    } catch {
      case _: RuntimeException =>
    }
  }

}
