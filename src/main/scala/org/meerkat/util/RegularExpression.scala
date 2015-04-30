package org.meerkat.util

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
  
  def apply(s: String) = StringPattern(s)
  
  implicit def toRegularExpression(s: String) = RegularExpression(s)
}
