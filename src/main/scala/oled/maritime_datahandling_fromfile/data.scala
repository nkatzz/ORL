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

package oled.maritime_datahandling_fromfile

import intervalTree.IntervalTree
import scala.collection.JavaConverters._

/**
  * Created by nkatz on 18/12/19.
  */
package object data {

  implicit class ITree[T](val tree: IntervalTree[T]) {

    def +=(from: Long, to: Long, data: T): Unit = tree.addInterval(from, to, data)

    def range(from: Long, to: Long): List[(Long, Long, T)] = {
      tree.getIntervals(from, to).asScala.toList.map { i =>
        if (from < i.getStart && to > i.getStart) (i.getStart, to, i.getData)
        else if (from >= i.getStart && to > i.getEnd) (from, i.getEnd, i.getData)
        else (from, to, i.getData)
      }
    }

  }

}
