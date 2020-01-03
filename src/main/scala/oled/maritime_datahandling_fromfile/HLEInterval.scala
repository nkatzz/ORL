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

/**
  * Created by nkatz on 18/12/19.
  */

object HLEInterval {

  // For me if argument is absent the row has space
  def apply(hleLine: String) = {
    val split = hleLine.split("\\|")
    val hle = split(0)

    /*
    val yours_event_set = Set("adrift", "aground", "atAnchor", "atAnchorOrMoored", "gap",
      "highSpeedNearCoast", "loitering", "lowSpeed", "maa", "moored", "speedGrThanMax", "tuggingSpeed",
      "speedLessThanMin", "stopped", "travelSpeed", "trawling", "trawlSpeed", "underWay", "unusualSpeed")
    */

    val mine_hle_event_set = Set("highSpeedNC", "lowSpeed", "stopped", "proximity", "trawlingMovement",
      "gap", "loitering", "changingSpeed", "tuggingSpeed", "trawling",
      "anchoredOrMoored", "sarSpeed", "sar", "trawlSpeed", "underWay",
      "movingSpeed", "drifting", "sarMovement", "pilotBoarding")

    // rendezVous, tugging
    if (mine_hle_event_set.contains(hle)) {
      val vessels = List(split(1))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "rendezVous" || hle == "tugging"
      || hle == "proximity" || hle == "pilotBoarding") {
      val vessels = List(split(1), split(2))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "withinArea") {
      //withinArea|923166|fishing|true|1448977130|1448977242
      val vessels = List(split(1), split(2))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else throw new RuntimeException(s"Don't know what to do with $hleLine")
  }
}

class HLEInterval(val hle: String, val vessels: List[String], val value: String, val stime: Long, val etime: Long)

