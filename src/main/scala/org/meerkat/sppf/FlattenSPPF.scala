package org.meerkat.sppf

object FlattenSPPF {
  
  def flatten(node: SPPFNode): List[SPPFNode] = node match {
    case n: NonterminalNode  => println(n); flattenChildren(n); List(n)
    case n: TerminalNode     => println(n); flattenChildren(n); List(n)
    case n: IntermediateNode => println(n); flattenChildren(n); flatten(n.first)
    case n: PackedNode       => println(n); flattenChildren(n); n.children.flatMap { x => flatten(x) }.toList
  }
  
  def flattenChildren(node: SPPFNode): Unit = node.children.foreach {x => flatten(x)}
  
}