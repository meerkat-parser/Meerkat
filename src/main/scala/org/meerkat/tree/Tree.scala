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

trait Tree {
  import Tree._  
  val id = inc
}

object Tree {
  private var id = 0
  private def inc = { id += 1; id }
  
  val epsilon = EpsilonNode()
  
  def isEpsilon(t: Tree): Boolean = t == epsilon  
}

trait RuleNode extends Tree {
  def r: Rule
  def ts: Seq[Tree]
}

case class RuleNodeImpl(val r: Rule, val ts: Seq[Tree]) extends RuleNode

object RuleNodeL {
  def unapply(n: RuleNode): Option[(Rule, Seq[Tree])] = Some((n.r, n.ts))
}

object RuleNode {
  def apply(r: Rule, ts: Seq[Tree])= new RuleNodeImpl(r, ts)
  def unapply(n: RuleNode): Option[(Rule, Seq[Tree])] = Some((n.r, n.ts filter { case l: LayoutNode => false; case _ => true }))
}

case class AmbNode(ts: Set[Tree]) extends Tree

case class TerminalNode(value: String) extends Tree

case class LayoutNode(value: Tree) extends Tree

case class EpsilonNode() extends Tree

