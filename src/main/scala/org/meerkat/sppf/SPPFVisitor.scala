package org.meerkat.sppf

object SPPFVisitor {
  
  def preOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { f(node); node.children.foreach {x => preOrder(x)(f)} }

  def postOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { node.children.foreach {x => postOrder(x)(f)}; f(node) }
    
  def flatten(node: SPPFNode): Unit = postOrder(node)(n => n match {
    case n: NonterminalNode  => 
    case n: TerminalNode     => 
    case n: IntermediateNode => 
    case n: PackedNode       => 
  })
  
}