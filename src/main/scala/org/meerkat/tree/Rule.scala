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

package org.meerkat.tree

trait Rule {
  def head: Nonterminal
  def body: Symbol
  
  var action: Option[Any => Any] = None 
  
  override def toString = head + " ::= " + body
}

object Rule {
  def apply(head: Nonterminal, body: Symbol) = DefaultRule(head, body)
}

case class DefaultRule(head: Nonterminal, body: Symbol) extends Rule

case class PartialRule(head: Nonterminal, body: Symbol, i: Int) extends Rule

case class RegularRule(head: Nonterminal) extends Rule {
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

case class Layout(name: String) extends Nonterminal {
  override def isRegular = false
}

case class Terminal(name: String) extends Symbol

trait Nonterminal extends Symbol {
  def name: String
  
  def isRegular: Boolean = true
}

object Nonterminal {
  def apply(s: String) = SimpleNonterminal(s)
  def unapply(s: Nonterminal): Option[String] = Some(s.name)
}

case class SimpleNonterminal(name: String) extends Nonterminal {
  override def isRegular = false
}

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
