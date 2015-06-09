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

import org.meerkat.parsers.Parsers.Terminal
import org.meerkat.sppf.SPPFLookup
import org.meerkat.parsers.CPSResult

trait RegularExpression {

  def ~(r: RegularExpression): RegularExpression = Seq(this, r)
  
  def |(r: RegularExpression): RegularExpression = Or(this, r)
  
  def *(): RegularExpression = Star(this)
  
  def +(): RegularExpression = Plus(this)
  
  def ?(): RegularExpression = Opt(this)

  override def toString: String = this match {
      case Char(c)           => escape(c)
      case Range(start, end) => "[" + start + "-" + end + "]"
      case StringPattern(s)  => "(?:" + s + ")"
      case Or(l, r)          => "(?:" + l + "|" + r + ")"
      case Seq(l, r)         => l.toString + r.toString
      case Opt(Seq(l, r))   => "(?:" + l.toString + r.toString + ")?"
      case Opt(r)            => r.toString + "?"
      case Star(Seq(l, r))   => "(?:" + l.toString + r.toString + ")*"
      case Star(r)           => r.toString + "*"
      case Plus(Seq(l, r))   => "(?:" + l.toString + r.toString + ")+"
      case Plus(r)           => r.toString() + "+"
   }
   
  def matcher: Matcher = new JavaRegexMatcher(toString)
  
  def escape(c: scala.Char): String = c match {
    case '*' => "\\*"
    case '+' => "\\+"
    case '$' => "\\$"
    case '^' => "\\^"
    case '&' => "\\&"
    case '[' => "\\["
    case ']' => "\\]"
    case '(' => "\\("
    case ')' => "\\)"
    case '.' => "\\."
    case x => x + "" 
  }
  
}

case class Or(l: RegularExpression, r: RegularExpression) extends RegularExpression

case class Seq(l: RegularExpression, r: RegularExpression) extends RegularExpression

case class Star(r: RegularExpression) extends RegularExpression

case class Plus(r: RegularExpression) extends RegularExpression

case class Opt(r: RegularExpression) extends RegularExpression

case class Char(c: scala.Char) extends RegularExpression {
  def --(c: Char): Range = Range(this, c)
}

object Char {
  implicit def toChar(c: scala.Char) = Char(c)
//  implicit def toSequence(s: String) = fromString(s)
  
//  def fromString(s: String): RegularExpression =
//   if (s.length == 0) throw new RuntimeException("Length cannot be zero")
//   else if(s.length() == 1) Char(s.charAt(0))
//   else { val x = s.foldLeft(Seq(s.charAt(0), s.charAt(1)))((seq, c) => Seq(seq, c)); 
//   x }
}


case class Range(start: Char, end: Char) extends RegularExpression

case class StringPattern(s: String) extends RegularExpression

trait Matcher {
  def next(input: Input, i: Int): Int
  def matches(input:Input, i: Int, j: Int): Boolean
  def matches(input:Input): Boolean = matches(input, 0, input.length)
}

class JavaRegexMatcher(s: String) extends Matcher {
  
  val matcher: java.util.regex.Matcher = s.r.pattern.matcher("")
  
  override def next(input: Input, i: Int): Int = {
    if (i < 0) return -1
    matcher.reset(input.s)
    matcher.region(i, input.length)
    if (matcher.lookingAt()) matcher.end 
    else -1
  }
  
  override def matches(input:Input, i: Int, j: Int): Boolean = {
    matcher.reset(input.s)
    matcher.region(i, j)
    matcher.matches()
  }
  
}

object RegularExpression {
  
  def apply(s: String) = StringPattern(s)
  
  implicit def toRegularExpression(s: String) = RegularExpression(s) 
  
//  implicit class ToRegularExpression(s: String) {
//    def re() = RegularExpression(s)  
//  }
  
}
