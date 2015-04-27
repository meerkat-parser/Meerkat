package org.meerkat.util

import java.util.regex.Matcher
import util.Input

trait RegularExpression {

  def pattern: String
  
  def next(input: Input, i: Int): Int
  
  def matches(input:Input, i: Int, j: Int): Boolean
  
  def matches(input:Input): Boolean = matches(input, 0, input.length)
  
  def ~(r: RegularExpression): RegularExpression = RegularExpression(pattern + r.pattern)
  
  def |(r: RegularExpression): RegularExpression = RegularExpression("(" + pattern + "|" + r.pattern + ")")
  
  def *(): RegularExpression = RegularExpression("(" + pattern + ")" + "*")
  
  def +(): RegularExpression = RegularExpression("(" + pattern + ")" + "+")
  
  def ?(): RegularExpression = RegularExpression("(" + pattern + ")" + "?")
}

class JavaRegularExpression(s: String) extends RegularExpression {
  
  val matcher: Matcher = s.r.pattern.matcher("")
  
  override def pattern: String = s
  
  override def next(input: Input, i: Int): Int = {
    matcher.reset(input.s)
    if (matcher.find(i))
      return i + matcher.end()
    else 
      return -1
  }
  
  override def matches(input:Input, i: Int, j: Int): Boolean = {
    matcher.reset(input.s)
    matcher.region(i, j)
    matcher.matches()
  }
}

object RegularExpression {
  
  def apply(pattern: String) = new JavaRegularExpression(pattern)
  
  implicit def toRegex(s: String) = RegularExpression(s)
}

