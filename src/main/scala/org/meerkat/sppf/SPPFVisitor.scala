package org.meerkat.sppf

import org.meerkat.meerkat.Rule
import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._

import scala.collection.breakOut


object SPPFVisitor {
  
  def preOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { f(node); node.children.foreach {x => preOrder(x)(f)} }

  def postOrder(node: SPPFNode)(f: SPPFNode => Unit): Unit = { node.children.foreach {x => postOrder(x)(f)}; f(node) }
   
  def buildTree(node: SPPFNode): Tree = node match {
    
    case t: TerminalNode    => if (t.leftExtent == t.rightExtent) epsilon 
                               else Terminal(t.name)
    
    case n: NonterminalNode => {
      if (n isAmbiguous)
        Amb( (for (p <- n.children) yield Appl(p.rule, for (c <- p.flatChildren) yield buildTree(c))) (breakOut) )
      else
    	  Appl(n.first.rule, n.flatChildren.map { x => buildTree(x) } toList)        
      }
    
    case i: IntermediateNode => {
      if (! i.isAmbiguous) throw new RuntimeException(s"$i should be ambiguous")
      Amb( (for (p <- i.children) yield Appl(p.rule, for (c <- p.flatChildren) yield buildTree(c))) (breakOut) )
    }
    
    case _                  => throw new RuntimeException("Should not reach here!")
  }
  
  
  def flatten(t: Tree): Tree = t match {
    case a @ Appl(r, ts) => r match {
      case Regular(s) => ???
      case _          => a
    }
    case x @ _          => x
  }
  
}