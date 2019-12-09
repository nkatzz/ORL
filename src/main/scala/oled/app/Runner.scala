package oled.app

import com.typesafe.scalalogging.LazyLogging

/**
 * Created by nkatz on 7/10/19.
 */

object Runner extends LazyLogging {

  def main(args: Array[String]) = {

    println("Hello world!")
    
    logger.error("ERROR")

    println("Hello again!")
  }

}



