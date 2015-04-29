package org.meerkat.sppf

import sppf.NonterminalNode
import sppf.PackedNode
import sppf.TerminalNode
import sppf.IntermediateNode
import sppf.SPPFNode
import sppf.NonterminalNode
import sppf.TerminalNode
import sppf.IntermediateNode
import sppf.PackedNode
import sppf.NonterminalNode

trait SPPFVisitor[T] {
  
  def visit(n: NonterminalNode): T
  def visit(n: TerminalNode): T
  def visit(n: IntermediateNode): T
  def visit(n: PackedNode): T  
  
  def visit(n: SPPFNode): T = n match {
    case n: NonterminalNode  => visitChildren(n); visit(n)
    case n: TerminalNode     => visit(n)
    case n: IntermediateNode => visit(n)
    case n: PackedNode       => visit(n)
  }
  
  def visitChildren(n: SPPFNode): Unit = n.children.foreach { x => visit(x) }
  
}

class UnambiguousSPPFVisitor extends SPPFVisitor[List[SPPFNode]] {

  def visit(n: NonterminalNode): List[SPPFNode] = 
    if (n.isAmbiguous) 
      throw new RuntimeException("Ambiguous")
    else 
      List(n)

  def visit(n: TerminalNode): List[SPPFNode] = List(n)
  
  def visit(n: IntermediateNode): List[SPPFNode] = 
    if (n.isAmbiguous) 
      throw new RuntimeException("Ambiguous")
    else 
      List(n)
  
  def visit(n: PackedNode): List[SPPFNode] = {
    ???
  }
}

object SPPFVisitor {
  
  def visitAll[T](n: NonterminalNode, f: Unit => T): T = {
    ???
  }
  
}