package org.meerkat.sppf

import org.meerkat.tree.DefaultTreeVisitor
import org.meerkat.meerkat.Rule
import org.meerkat.tree.Tree

object SPPFVisitor {
  
  def preOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { f(node); node.children.foreach {x => preOrder(x)(f)} }

  def postOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { node.children.foreach {x => postOrder(x)(f)}; f(node) }
  
}