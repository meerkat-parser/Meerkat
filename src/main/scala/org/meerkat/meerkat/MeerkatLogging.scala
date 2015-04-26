/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import sppf.NonPackedNode
import util.LoggerWrapper
import util.JavaUtilLoggerWrapper

object MeerkatLogging {
  
  var logger: LoggerWrapper = new JavaUtilLoggerWrapper()
  var counter = 0
    
  def inSequence(t: NonPackedNode, k: => String): Unit = {
    if(logger.isDEBUG) {
      logger.debug("%s.%s, %d, (<head>, %d ), ( %s )", t.name.toString, k, new Integer(t.rightExtent), new Integer(t.leftExtent), t)
      counter += 1
    }
  }
    
  def beginOfAlt(head: String, alt: => String, i: Int): Unit = {
    if(logger.isDEBUG) {
      logger.debug(".%s, %d, (%s, %s), $", alt, new Integer(i), head, new Integer(i))
      counter += 1
    }
  }
      
  def endOfAlt(head: String, alt: => String, i: Int, t: NonPackedNode): Unit = {
    if(logger.isDEBUG) {
      logger.debug("%s., (%s, %d), ( %s )", alt, head, new Integer(i), t)
      counter += 1
    }
  }
      
  def reset(): Unit = {
    logger = new JavaUtilLoggerWrapper()
    counter = 0
  }
    
}