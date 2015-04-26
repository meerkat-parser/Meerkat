/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

import scala.util.matching.Regex
import java.util.regex.Matcher
import scala.collection.mutable._
import scala.collection.JavaConversions._

class Input(s: String) {
  
  def length = s.length()

  val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)
  
  val regexMap: Map[Regex, Matcher] = new java.util.HashMap[Regex, Matcher]()
  
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
  
  def charAt(i: Int): Char = {
    s.charAt(i)
  } 
  
  def substring(start: Int, end: Int): String = s.substring(start, end)
  
  def startsWith(prefix: String, toffset: Int) = s.startsWith(prefix, toffset)
  
  def endsWith(suffix: String) = s.endsWith(suffix)
  
  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if(start < 0) return false
    val matcher = regexMap.getOrElse(r, {val matcher: Matcher = r.pattern.matcher(s); matcher})
    matcher.region(start, end)
    return matcher.matches()
  }
  
  def matchRegex(r: Regex, start: Int): Int = {
    if(start < 0) return -1
    val matcher = regexMap.getOrElse(r, {val matcher: Matcher = r.pattern.matcher(s); matcher})
    matcher.region(start, length)
    if (matcher.lookingAt()) matcher.end else -1
  }
  
  def lineNumber(i: Int): Int  = lineColumns(i)._1
  
  def columnNumber(i: Int): Int = lineColumns(i)._2
  
  def lineColumn(i: Int): (Int, Int) = lineColumns(i)
  
}