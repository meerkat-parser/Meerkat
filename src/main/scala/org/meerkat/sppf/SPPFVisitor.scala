package org.meerkat.sppf

import org.meerkat.meerkat.Rule
import org.meerkat.tree.Tree
import org.meerkat.tree._

import scala.collection.breakOut


object SPPFVisitor {
  
  def preOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { f(node); node.children.foreach {x => preOrder(x)(f)} }

  def postOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { node.children.foreach {x => postOrder(x)(f)}; f(node) }
   
  def buildTree(node: SPPFNode): Tree = node match {
    case t: TerminalNode    => Terminal(t.name)
    case n: NonterminalNode => {
      if (n isAmbiguous) 
        Amb( (for (p <- n.children) yield Appl(p.rule, for (c <- p.children) yield buildTree(c))) (breakOut) )
      else 
        Appl(n.first.rule, n.flatChildren.map { x => buildTree(x) } toList)
      }
    case _                  => throw new RuntimeException("Should not reach here!")
  }

}