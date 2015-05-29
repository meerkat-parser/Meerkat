package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input
import scala.collection.mutable.HashMap

trait SPPFVisitor {
  type T
  def visit(node: NonPackedNode)(implicit input: Input): T
}

trait Memoization extends SPPFVisitor {
  
  val cache = new HashMap[NonPackedNode, T]
  
  override abstract def visit(node: NonPackedNode)(implicit input: Input): T =
    cache.getOrElseUpdate(node, super.visit(node))
}

trait EBNFList
case class StarList(s: Symbol, l: List[Any]) extends EBNFList
case class PlusList(s: Symbol, l: List[Any]) extends EBNFList


class SemanticActionExecutor(amb: Set[Any] => Any,
                             tn : String => Any,
                             nt2: (RuleType, (Any, Any)) => Any, 
                             nt1: (RuleType, Any) => Any) extends SPPFVisitor {
 
   type T = Any
  
   def ambiguity(n: NonPackedNode)(implicit input: Input): Any =  
     amb((for (p <- n.children) yield nonterminal(p)) (breakOut))
   
   def flatten2(p: PackedNode, l: Any, r: Any) = p.ruleType.head match {
     case Star(s) => l match {
       case StarList(s, xs) => StarList(s, xs :+ r)
       case x: Any  => StarList(p.ruleType.head, List(l, r))
     }
     case Plus(s) => l match {
       case PlusList(s, xs) => PlusList(s, xs :+ r)
       case x:  Any      => PlusList(p.ruleType.head, List(x))
     }
     case _  => nt2(p.ruleType, (l, r))
   }
      
   def flatten1(p: PackedNode, c: Any) = {
     p.ruleType.head match {
       case Star(s) => c match { case Nil => StarList(s, List()); case _ =>  StarList(s, List(c)) } 
       case Plus(s) => c match { case Nil => PlusList(s, List()); case _ => PlusList(s, List(c)) }
       case _ => nt1(p.ruleType, c)
     }
   }
   
   def nonterminal(p: PackedNode)(implicit input: Input): Any = {
     if (p.hasRightChild) {
       val left  = visit(p.leftChild)
       val right = visit(p.rightChild)
       flatten2(p, left, right)
     }
     else {
       val child = visit(p.leftChild)
       flatten1(p, child)
     }
   }
  
  def visit(node: NonPackedNode)(implicit input: Input): Any = node match {
     case t: TerminalNode     => if (t.leftExtent == t.rightExtent) Nil 
                                 else tn(input.substring(t.leftExtent, t.rightExtent))
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) else nonterminal(n.first)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) else ((visit(i.first.leftChild), visit(i.first.rightChild)))  
  } 
  
} 

object TreeBuilder {

   def convert(t: Any): Tree = t match {
    case StarList(s, xs) => Appl(RegularRule(Star(s)), xs.asInstanceOf[Seq[Tree]]) 
    case PlusList(s, xs) => Appl(RegularRule(Plus(s)), xs.asInstanceOf[Seq[Tree]])
    case _               => t.asInstanceOf[Tree]
  }
  
  def flatten(t: Any): Seq[Any] = t match {
    case (t: (_, _), y) => flatten(t) :+ convert(y)
    case (x, y) => List(convert(x), convert(y))
    case x      => List(convert(x))
  } 
  
  def amb(s: Set[Any]): Tree = Amb(s.asInstanceOf[Set[Tree]]) 
  def t(s: String): Tree = Terminal(s)
  def nt2(r: RuleType, t: (Any, Any)) = Appl(r, flatten(t).asInstanceOf[Seq[Tree]])
  def nt1(r: RuleType, t: Any) = Appl(r, flatten(t).asInstanceOf[Seq[Tree]])

  
  def newBuilder: SPPFVisitor = {
    new SemanticActionExecutor(amb, t, nt2, nt1)
  }
  
  def newMemoBuilder: SPPFVisitor = {
    new SemanticActionExecutor(amb, t, nt2, nt1) with Memoization
  }
  
  def build(node: NonPackedNode, memoized: Boolean = true)(implicit input: Input): Tree =
    if (memoized)
      newMemoBuilder.visit(node).asInstanceOf[Tree]
    else 
      newBuilder.visit(node).asInstanceOf[Tree]
  
}
  
  