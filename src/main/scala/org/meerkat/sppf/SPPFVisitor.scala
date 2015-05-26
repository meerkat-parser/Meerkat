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
   
   def nonterminal(p: PackedNode): Any = if (p.hasRightChild) 
                                           nt2(p.ruleType, (visit(p.leftChild, amb, tn, nt2, nt1), visit(p.rightChild, amb, tn, nt2, nt1)))
                                         else 
                                           nt1(p.ruleType, visit(p.leftChild, amb, tn, nt2, nt1))
    
   node match {
    
     case t: TerminalNode     => tn(input.substring(t.leftExtent, t.rightExtent))
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) else nonterminal(n.first)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) else ((visit(i.first.leftChild, amb, tn, nt2, nt1), visit(i.first.rightChild, amb, tn, nt2, nt1)))  
                                 
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
      
//        def merge(p: PackedNode): List[Tree] = if (p.hasRightChild)
//                                               List(buildTree(p.leftChild), buildTree(p.rightChild))
//                                             else
//                                               List(buildTree(p.leftChild))
//                                               
//      def amb(n: NonPackedNode): Tree =  Amb((for (p <- n.children) yield Appl(p.ruleType, merge(p))) (breakOut))                                         
//      
//      def nt(p: PackedNode): Tree = Appl(p.ruleType, merge(p))
  

  def buildTree(node: NonPackedNode)(implicit input: Input): Tree = {
    def amb(s: Set[Any]): Tree = Amb(s.asInstanceOf[Set[Tree]]) 
    def t(s: String): Tree = Terminal(s)
    def nt2(r: RuleType, t: (Any, Any)) = Appl(r, flatten(t).asInstanceOf[Seq[Tree]])
    def nt1(r: RuleType, t: Any) = Appl(r, List(t.asInstanceOf[Tree]))
    visit(node, amb, t, nt2, nt1).asInstanceOf[Tree]
  }
  
  def flatten(t: (Any, Any)): Seq[Any] = t match {
    case (t: (_, _), y) => flatten(t) :+ y
    case (x, y) => List(x, y)
  } 
  
  
  def flatten(t: Tree): Tree = t match {
      
      case Appl(r@Rule(Star(s), _), List(epsilon)) => Appl(r, List())
      
      case Appl(r@Rule(Star(s), _), Appl(Rule(Star(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case Appl(r@Rule(Plus(s), _), Appl(Rule(Plus(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case a @ _          => a
  }
  
}