/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.util

import java.util.logging._

trait LoggerWrapper {

  def debug(message: => String): Unit;
  def debug(message: String, a: Object): Unit = debug(String.format(message, a))
  def debug(message: String, a1: Object, a2: Object): Unit = debug(String.format(message, a1, a2))
  def debug(message: String, a1: Object, a2: Object, a3: Object): Unit = debug(String.format(message, a1, a2, a3))
  def debug(message: String, a1: Object, a2: Object, a3: Object, a4: Object): Unit = debug(String.format(message, a1, a2, a3, a4))
  def debug(message: String, a1: Object, a2: Object, a3: Object, a4: Object, a5: Object): Unit = debug(String.format(message, a1, a2, a3, a4, a5))


  def info(message: => String): Unit;
  def info(message: String, a: Object): Unit = info(String.format(message, a));
  def info(message: String, a1: Object, a2: Object): Unit = info(String.format(message, a1, a2))
  def info(message: String, a1: Object, a2: Object, a3: Object): Unit = info(String.format(message, a1, a2, a3))
  def info(message: String, a1: Object, a2: Object, a3: Object, a4: Object): Unit = info(String.format(message, a1, a2, a3, a4))
  def info(message: String, a1: Object, a2: Object, a3: Object, a4: Object, a5: Object): Unit = info(String.format(message, a1, a2, a3, a4, a5))
  
  def setLevel(level: LogLevel);
  
  def isDEBUG: Boolean
  
}

sealed trait LogLevel { def name: String }
case object INFO extends LogLevel { val name = "INFO" }
case object DEBUG extends LogLevel { val name = "DEBUG" }
case object SEVERE extends LogLevel { val name = "SEVERE" }

class JavaUtilLoggerWrapper() extends LoggerWrapper {

  val logger: Logger = Logger.getLogger("logger")
  var isDebug: Boolean = false
  
  def isDEBUG = this.isDebug

  logger.setUseParentHandlers(false);
  val handler: ConsoleHandler = new ConsoleHandler();
  
  // Removing all the registered handlers
  for (h:Handler <- logger.getHandlers()) {
	  logger.removeHandler(h)    
  }
  logger.addHandler(handler);
  handler.setFormatter(new ParserLogFormatter());

  override def debug(message: => String) = {
    if (logger.isLoggable(Level.FINE)) logger.fine(message);
  }

  override def info(message: => String) = {
    if (logger.isLoggable(Level.INFO)) { logger.info(message); }
  }
  
  override def setLevel(level: LogLevel) = {
    level match {
      case INFO => 	logger.setLevel(Level.INFO);
      				handler.setLevel(Level.INFO);
      				
      case DEBUG => logger.setLevel(Level.FINE);
      				handler.setLevel(Level.FINE);
      				isDebug = true
      				
      case SEVERE => logger.setLevel(Level.SEVERE);
      				 handler.setLevel(Level.SEVERE);
    }
  } 
}

class ParserLogFormatter extends Formatter {
  override def format(record: LogRecord): String = record.getMessage() + "\n"
}

