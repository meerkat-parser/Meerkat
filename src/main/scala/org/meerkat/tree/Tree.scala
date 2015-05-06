package org.meerkat.tree

trait Symbol {
  def name: String
}

case class Nonterminal(name: String) extends Symbol

case class Terminal(name: String) extends Symbol

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

case class Rule(head: Nonterminal, body: List[Symbol])



case class Amb(rs: Set[Rule])
