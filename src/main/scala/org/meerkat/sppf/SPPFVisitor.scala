package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input
import scala.collection.mutable.HashMap

trait SPPFVisitor {
  type T
  def visit(node: SPPFNode)(implicit input: Input): T
}

trait Memoization extends SPPFVisitor {
  
  val cache = new HashMap[SPPFNode, T]
  
  override abstract def visit(node: SPPFNode)(implicit input: Input): T =
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
  
  def visit(node: SPPFNode)(implicit input: Input): Any = node match {
     case t: TerminalNode     => if (t.leftExtent == t.rightExtent) Nil 
                                 else tn(input.substring(t.leftExtent, t.rightExtent))
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) else nonterminal(n.first)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) else ((visit(i.first.leftChild), visit(i.first.rightChild)))
     
     case p: PackedNode       => throw new RuntimeException("Should not traverse a packed node!")
  } 
}

class SPPFToDot extends SPPFVisitor {
  type T = Unit
  
  def get: String = sb.toString
  
  import org.meerkat.util.visualization._
  import org.meerkat.util.visualization.Shape._
  import org.meerkat.util.visualization.Style._
  
  val sb = new StringBuilder
  
  def visit(node: SPPFNode)(implicit input: Input): T = 
    node match {
      case n@NonterminalNode(slot, leftExtent, rightExtent) => 
        sb ++= getShape(n.toString(), s"($slot, $leftExtent, $rightExtent)", Rectangle, Rounded)
        for(t <- n.children) visit(t)
        for(t <- n.children) addEdge(n.toString, t.toString, sb)
                    
      case n@IntermediateNode(slot, leftExtent, rightExtent) =>
        sb ++= getShape(n.toString(), s"$slot, $leftExtent, $rightExtent", Rectangle)
        for(t <- n.children) visit(t)
        for(t <- n.children) addEdge(n.toString, t.toString, sb)
                    
      case n@TerminalNode(char, leftExtent, rightExtent) =>
        sb ++= getShape(n.toString, char.toString, Rectangle, Rounded)
        sb ++= s""""${escape(n.toString)}"[shape=box, style=rounded, height=0.1, width=0.1, color=black, fontcolor=black, label="(${escape(char)}, $leftExtent, $rightExtent)", fontsize=10];\n"""
        
      case n@PackedNode(slot, parent) =>
//      sb ++= getShape(n.toString, s"($slot, ${n.pivot})", Diamond)
        sb ++= getShape(n.toString, "", Diamond)
        for(t <- n.children) {
          visit(t)
          addEdge(n.toString, t, sb)
        }
    }
}

object SemanticAction {
  def amb(s: Set[Any]) = throw new RuntimeException 
  def t(s: String) = s
  def nt2(r: RuleType, t: (Any, Any)) = r.action(t)
  def nt1(r: RuleType, t: Any) = r.action(t)
  
  def execute(node: NonPackedNode)(implicit input: Input) =
    new SemanticActionExecutor(amb, t, nt2, nt1).visit(node)
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
  
  