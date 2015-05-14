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
        Amb( (for (p <- n.children) yield Appl(p.ruleType, for (c <- p.flatChildren) yield buildTree(c))) (breakOut) )
      else
    	  Appl(n.first.ruleType, n.flatChildren.map { x => buildTree(x) } toList)        
      }
    
    case i: IntermediateNode => {
      if (! i.isAmbiguous) throw new RuntimeException(s"$i should be ambiguous")
      Amb( (for (p <- i.children) yield Appl(p.ruleType, for (c <- p.flatChildren) yield buildTree(c))) (breakOut) )
    }
    
    case _                  => throw new RuntimeException("Should not reach here!")
  }
  
  
//  def flatten(t: Tree): Tree = t match {
//    case a @ Appl(r, ts) => r match {
//      
//      // TODO: we only need to check for the right most or left most node, so iterating over the list
//      // of children may not be the optimal way to go.
//      case Regular(Star(s)) => Appl(r, ts.filter(x => isEpsilon(x)).flatMap{ x => x match { case Appl(Regular(Star(s)), xs) => xs } })
//      
//      case Regular(Plus(s)) => Appl(r, ts.filter(x => isEpsilon(x)).flatMap{ x => x match { case Appl(Regular(Plus(s)), xs) => xs } })
//      
//      case Regular(Opt(s))  => Appl(r, ts.filter { x => isEpsilon(x) })
//      
//      case _          => a
//    }
//    case x @ _          => x
//  }
  
}