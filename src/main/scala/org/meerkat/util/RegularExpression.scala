package org.meerkat.util

import java.util.regex.Matcher
import util.Input

trait RegularExpression {
  def next(input: Input, i: Int): Int
  def matches(input:Input, i: Int, j: Int): Boolean
}

class JavaRegularExpression(pattern: String) extends RegularExpression {
  
  val matcher: Matcher = pattern.r.pattern.matcher("")
  
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
}