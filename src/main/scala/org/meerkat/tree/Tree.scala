package org.meerkat.tree

import org.meerkat.sppf.SPPFNode
import org.meerkat.sppf.TerminalNode
import org.meerkat.sppf.NonterminalNode

trait Tree

case class Appl(r: AbstractRule, ts: Seq[Tree]) extends Tree

case class Amb(ts: Set[Tree]) extends Tree

trait AbstractRule {
  def head: Nonterminal
  def body: Seq[Symbol]
}

case class Rule(head: Nonterminal, body: Seq[Symbol]) extends AbstractRule

case class PartialRule(head: Nonterminal, body: Seq[Symbol], i: Int) extends AbstractRule

object PartialRule {
  def apply(head: Nonterminal, body: Seq[Symbol]): PartialRule = PartialRule(head, body, body.length)
}

trait Symbol {
  def name: String
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
