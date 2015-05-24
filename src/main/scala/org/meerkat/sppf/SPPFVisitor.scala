package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input


object SPPFVisitor {
  
  def visit(node: NonPackedNode, amb: NonPackedNode => Any, nt: PackedNode => Any, merge: PackedNode => Any)(implicit input: Input): Any = {
    
   node match {
    
     case t: TerminalNode     => input.substring(t.leftExtent, t.rightExtent)
    
     case n: NonterminalNode  => if (n isAmbiguous) amb(n) else nt(n.first)
                                   
     case i: IntermediateNode => if (i isAmbiguous) amb(i) else merge(i.first) 
                                 
     case p: PackedNode       => throw new RuntimeException("Should not end up here!")
   }
    
  }
  
//  def concat(node: SPPFNode)(implicit input: Input): String = {
//    def terminal(s: String): String = s
//    def nonterminal(t: RuleType, children: Seq[String]): String = children mkString(",")
//    def amb(children: Set[String]): String = throw new RuntimeException()
//    visit(node, nonterminal, terminal, amb)
//  }
//  
//  def buildTree(node: SPPFNode)(implicit input: Input): Tree = {
//    def terminal(s: String): Tree = Terminal(s)
//    def nonterminal(t: RuleType, children: Seq[Tree]): Tree = Appl(t, children)
//    def amb(children: Set[Tree]): Tree = Amb(children)
//    visit(node, nonterminal, terminal, amb)
//  }
  
  
      //def ambiguity(n: NonPackedNode): Any =  amb(for (p <- n.children) yield merge(p)) 
      

  def buildTree(node: NonPackedNode)(implicit input: Input): Tree = {
    
      def merge(p: PackedNode): List[Tree] = if (p.hasRightChild)
                                               List(buildTree(p.leftChild), buildTree(p.rightChild))
                                             else
                                               List(buildTree(p.leftChild))
                                               
      def amb(n: NonPackedNode): Tree =  Amb((for (p <- n.children) yield Appl(p.ruleType, merge(p))) (breakOut))                                         
      
      def nt(p: PackedNode): Tree = Appl(p.ruleType, merge(p))
    
      visit(node, amb, nt, merge).asInstanceOf[Tree]
  }
  
  
  def flatten(t: Tree): Tree = t match {
      
      case Appl(r@Rule(Star(s), _), List(epsilon)) => Appl(r, List())
      
      case Appl(r@Rule(Star(s), _), Appl(Rule(Star(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case Appl(r@Rule(Plus(s), _), Appl(Rule(Plus(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case a @ _          => a
  }
  
}