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

import scala.util.matching.Regex
import scala.collection.mutable._
import scala.collection.JavaConversions._

class Input(val s: String) {
  
  def length = s.length()

  val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)
  
  val regexMap: Map[Regex, java.util.regex.Matcher] = new java.util.HashMap[Regex, java.util.regex.Matcher]()
  
  calcLineColumns
  
  def calcLineColumns: Unit = {
    var lineCount = 0
	  var lineNumber = 1
	  var columnNumber = 1

	  // Empty input: only the end of line symbol
  	if(length == 1) {
  		lineColumns(0) = (lineNumber, columnNumber)
  	} else {
   		for (i <- 0 until length) {
  			lineColumns(i) = (lineNumber, columnNumber)
  			if (s.charAt(i) == '\n') {
  				lineCount += 1
  				lineNumber += 1
  				columnNumber = 1
  			} else if (s.charAt(i) == '\r') {
  				columnNumber = 1
  			} else {
  				columnNumber += 1
  			}
  		}
  	}
    lineColumns(length) = (lineNumber, columnNumber)
  }
  
  def charAt(i: Int): scala.Char = s.charAt(i)
  
  def substring(start: Int, end: Int): String = s.substring(start, end)
  
  def startsWith(prefix: String, toffset: Int) = s.startsWith(prefix, toffset)
  
  def endsWith(suffix: String) = s.endsWith(suffix)
  
  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if(start < 0) return false
    val matcher = regexMap.getOrElse(r, {val matcher: java.util.regex.Matcher = r.pattern.matcher(s); matcher})
    matcher.region(start, end)
    return matcher.matches()
  }
  
  def matchRegex(r: Regex, start: Int): Int = {
    if(start < 0) return -1
    val matcher = regexMap.getOrElse(r, {val matcher: java.util.regex.Matcher = r.pattern.matcher(s); matcher})
    matcher.region(start, length)
    if (matcher.lookingAt()) matcher.end else -1
  }
  
  def lineNumber(i: Int): Int  = lineColumns(i)._1
  
  def columnNumber(i: Int): Int = lineColumns(i)._2
  
  def lineColumn(i: Int): (Int, Int) = lineColumns(i)
  
}


object Input {
  
  def apply(s:String) = new Input(s)
  
  implicit def toInput(s: String) = Input(s)
}
