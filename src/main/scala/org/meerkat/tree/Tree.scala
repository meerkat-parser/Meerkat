package org.meerkat.tree

import org.meerkat.sppf.SPPFNode
import org.meerkat.sppf.TerminalNode
import org.meerkat.sppf.NonterminalNode

trait Tree {
  import Tree._  
  val id = inc
}

object Tree {
  private var id = 0
  private def inc = { id += 1; id }
  
  val epsilon = Epsilon()
  
  def isEpsilon(t: Tree): Boolean = t == epsilon  
}

case class Appl(r: RuleType, ts: Seq[Tree]) extends Tree {
  override def toString = r.toString
}

case class Amb(ts: Set[Tree]) extends Tree

case class Epsilon() extends Tree

trait RuleType {
  def head: Nonterminal
  def body: Symbol
  
  var action: Option[Any => Any] = None 
  
  override def toString = head + " ::= " + body
} 

case class Rule(head: Nonterminal, body: Symbol) extends RuleType

case class PartialRule(head: Nonterminal, body: Symbol, i: Int) extends RuleType

case class RegularRule(head: Nonterminal) extends RuleType {
  def body = Sequence()
  
  override def toString = head.name
}

object PartialRule {
  def apply(head: Nonterminal, body: Symbol): PartialRule = PartialRule(head, body, 0)
}

trait Symbol {
  def name: String
  
  override def toString = name
}

trait Nonterminal extends Symbol {
  def name: String
  
  def isRegular: Boolean = true
}

object Nonterminal {
  def apply(s: String) = SimpleNonterminal(s)
}

case class SimpleNonterminal(name: String) extends Nonterminal {
  override def isRegular: Boolean = false
}

case class Terminal(name: String) extends Symbol with Tree

case class Star(s: Symbol) extends Nonterminal {
  override def name = s.name + "*"
}

case class Plus(s: Symbol) extends Nonterminal {
  override def name = s.name + "+"
}

case class Sequence(ss: Symbol*) extends Nonterminal {
  override def name = ss mkString " "
}

case class Group(s: Symbol) extends Nonterminal {
  override def name = "(" + s + ")"
}

case class Opt(s: Symbol) extends Nonterminal {
  override def name = s.name + "?"
}

case class Alt(s1: Symbol, s2: Symbol) extends Nonterminal {
  override def name = s1.name + "|" + s2.name
}
