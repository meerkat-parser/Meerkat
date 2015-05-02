package org.meerkat.sppf

object FlattenSPPF {
  
  def flatten(node: SPPFNode): List[SPPFNode] = node match {
    case n: NonterminalNode  => List(n)
    case n: TerminalNode     => List(n)
    case n: IntermediateNode => flatten(n.first)
    case n: PackedNode       => List(flatten(n.leftChild), flatten(n.rightChild)).flatten
  }
  
  
}