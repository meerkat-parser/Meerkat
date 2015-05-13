package org.meerkat.tree

import org.meerkat.sppf.SPPFNode
import org.meerkat.sppf.TerminalNode
import org.meerkat.sppf.NonterminalNode

trait Tree {
  import Tree._  
  val id = inc

  override def equals(o: Any) = o match {
    case t: Tree => id == t.id
    case _       => false
  }
  
  override def hashCode = id
}

object Tree {
  private var id = 0
  private def inc = { id += 1; id }
  
  val epsilon = Epsilon()
}

case class Appl(r: RuleType, ts: Seq[Tree]) extends Tree {
  override def toString = r.toString
}

case class Amb(ts: Set[Tree]) extends Tree

case class Epsilon() extends Tree

trait RuleType 

trait AbstractRule extends RuleType{
  def head: Nonterminal
  def body: Seq[Symbol]
  
  override def toString = head + " ::= " + body.mkString(" ")
}

case class Rule(head: Nonterminal, body: Seq[Symbol]) extends AbstractRule

case class PartialRule(head: Nonterminal, body: Seq[Symbol], i: Int) extends AbstractRule

case class Regular(s: Symbol) extends RuleType {
  override def toString = s.toString
}

object PartialRule {
  def apply(head: Nonterminal, body: Seq[Symbol]): PartialRule = PartialRule(head, body, body.length)
}

trait Symbol {
  def name: String
  
  override def toString = name
}

case class Nonterminal(name: String) extends Symbol

case class Terminal(name: String) extends Symbol with Tree

case class Star(s: Symbol) extends Symbol {
  override def name = s.name + "*"
}

case class Plus(s: Symbol) extends Symbol {
  override def name = s.name + "+"
}

case class Group(ss: List[Symbol]) extends Symbol {
  override def name = "(" + ss.map { _.name }.mkString + ")"
}

case class Opt(s: Symbol) extends Symbol {
  override def name = s.name + "?"
}

case class Alt(s1: Symbol, s2: Symbol) extends Symbol {
  override def name = s1.name + "|" + s2.name
}
