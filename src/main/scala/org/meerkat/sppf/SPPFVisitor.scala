package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input


object SPPFVisitor {
  
  def visit(node: NonPackedNode, 
            amb: Set[Any] => Any,
            tn : String => Any,
            nt2: (RuleType, (Any, Any)) => Any, 
            nt1: (RuleType, Any) => Any)(implicit input: Input): Any = {
    
   def ambiguity(n: NonPackedNode): Any =  amb( (for (p <- n.children) yield nonterminal(p)) (breakOut))
   
   def shouldFlatten(p: PackedNode) = p.ruleType.head match {
       case Star(s) => !p.leftChild.isAmbiguous && p.leftChild.first != null && p.leftChild.first.ruleType.head == Star(s)
       case Plus(s) => !p.leftChild.isAmbiguous && p.leftChild.first != null && p.leftChild.first.ruleType.head == Plus(s)
       case _       => false
   }
      
   def nonterminal(p: PackedNode): Any = {
     if (p.hasRightChild) {
       val left  = visit(p.leftChild, amb, tn, nt2, nt1)
       val right = visit(p.rightChild, amb, tn, nt2, nt1)
       if(shouldFlatten(p)) 
         (left, right) 
       else 
         nt2(p.ruleType, (left, right)) 
     }
     else {
       val child = visit(p.leftChild, amb, tn, nt2, nt1)
       if (shouldFlatten(p)) 
         child 
       else 
         nt1(p.ruleType, child)
     }
   }
   
   node match {
     case t: TerminalNode     => tn(input.substring(t.leftExtent, t.rightExtent))
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) else nonterminal(n.first)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) else ((visit(i.first.leftChild, amb, tn, nt2, nt1), visit(i.first.rightChild, amb, tn, nt2, nt1)))  
                                 
     case p: PackedNode       => throw new RuntimeException("Should not end up here!")
   }
    
  }
  
  def buildTree(node: NonPackedNode)(implicit input: Input): Tree = {
    def amb(s: Set[Any]): Tree = Amb(s.asInstanceOf[Set[Tree]]) 
    def t(s: String): Tree = Terminal(s)
    def nt2(r: RuleType, t: (Any, Any)) = Appl(r, flatten(t).asInstanceOf[Seq[Tree]])
    def nt1(r: RuleType, t: Any) = Appl(r, flatten(t).asInstanceOf[Seq[Tree]])
    visit(node, amb, t, nt2, nt1).asInstanceOf[Tree]
  }
  
  def flatten(t: Any): Seq[Any] = t match {
    case (t: (_, _), y) => flatten(t) :+ y
    case (x, y) => List(x, y)
    case x      => List(x)
  } 
  
}