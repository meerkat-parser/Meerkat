/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

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

