package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input


object SPPFVisitor {
  
  def visit[T](node: SPPFNode, nonterminal: (RuleType, Seq[T]) => T, terminal: String => T, amb: Set[T] => T)(implicit input: Input): T = {
     
   def ambiguity(node: NonPackedNode): T = 
     amb((for (p <- node.children) yield nonterminal(p.ruleType, for (c <- p.flatChildren) yield visit(c, nonterminal, terminal, amb))) (breakOut) )

   node match {
    
     case t: TerminalNode     => terminal(input.substring(t.leftExtent, t.rightExtent))
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n)
                                 else nonterminal (n.first.ruleType, n.flatChildren.map (n => visit(n, nonterminal, terminal, amb)))
      
     case i: IntermediateNode => if (! i.isAmbiguous) throw new RuntimeException(s"$i should be ambiguous") 
                                 else ambiguity(i)
                                 
     case _                   => throw new RuntimeException("Should not reach here!")
   }
    
  }
  
  def concat(node: SPPFNode)(implicit input: Input): String = {
    def terminal(s: String): String = s
    def nonterminal(t: RuleType, children: Seq[String]): String = children mkString(",")
    def amb(children: Set[String]): String = throw new RuntimeException()
    visit(node, nonterminal, terminal, amb)
  }
  
  def buildTree(node: SPPFNode)(implicit input: Input): Tree = {
    def terminal(s: String): Tree = Terminal(s)
    def nonterminal(t: RuleType, children: Seq[Tree]): Tree = Appl(t, children)
    def amb(children: Set[Tree]): Tree = Amb(children)
    visit(node, nonterminal, terminal, amb)
  }
  
  
  def flatten(t: Tree): Tree = t match {
      
      case Appl(r@Rule(Star(s), _), List(epsilon)) => Appl(r, List())
      
      case Appl(r@Rule(Star(s), _), Appl(Rule(Star(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case Appl(r@Rule(Plus(s), _), Appl(Rule(Plus(c), _), first) :: rest) => if (s == c) Appl(r, first ++ rest) else t
      
      case a @ _          => a
  }
  
}