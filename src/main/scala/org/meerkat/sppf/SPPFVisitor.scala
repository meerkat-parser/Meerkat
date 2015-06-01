package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input
import scala.collection.mutable.HashMap

trait SPPFVisitor {
  type T
  def visit(node: SPPFNode): T
}

trait Memoization extends SPPFVisitor {
  
  val cache = new HashMap[SPPFNode, T]
  
  override abstract def visit(node: SPPFNode): T =
    cache.getOrElseUpdate(node, super.visit(node))
}

trait EBNFList
case class StarList(s: Symbol, l: List[Any]) extends EBNFList
case class PlusList(s: Symbol, l: List[Any]) extends EBNFList
case class OptList(s: Symbol, l: List[Any])  extends EBNFList

class SemanticActionExecutor(amb: (Set[Any], Int, Int) => Any,
                             tn : (Int, Int) => Any,
                             int: (RuleType, Any, Any) => Any,
                             nt:  (RuleType, Any, Int, Int) => Any) extends SPPFVisitor {
 
   type T = Any
  
   def ambiguity(n: NonPackedNode): Any =  
     amb((for (p <- n.children) yield nonterminal(p, n.leftExtent, n.rightExtent)) (breakOut), n.leftExtent, n.rightExtent)
   
   def flatten(p: PackedNode, v: Any, leftExtent: Int, rightExtent: Int) = 
     p.ruleType.head match {
       case Star(s) => v match {
         case ()                   => StarList(s, List())
         case (StarList(s, xs), r) => StarList(s, xs :+ r)
         case x: Any               => StarList(s, List(x))
       }
       case Plus(s) => v match {
         case ()                   => PlusList(s, List())
         case (PlusList(s, xs), r) => PlusList(s, xs :+ r)
         case x:  Any              => PlusList(s, List(x))
       }
       case Opt(s) => v match {
         case ()                   => OptList(s, List())
         case x: Any               => OptList(s, List(x))
       }
       case _  => nt(p.ruleType, v, leftExtent, rightExtent)
     }
   
   def nonterminal(p: PackedNode, leftExtent: Int, rightExtent: Int): Any = 
      flatten(p, visit(p.leftChild), leftExtent, rightExtent)
  
  def visit(node: SPPFNode): Any = node match {
     case t: TerminalNode     => if (t.leftExtent == t.rightExtent) () 
                                 else tn(t.leftExtent, t.rightExtent)
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) else nonterminal(n.first, n.leftExtent, n.rightExtent)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) else int(i.first.ruleType, visit(i.first.leftChild), visit(i.first.rightChild))
     
     case p: PackedNode       => throw new RuntimeException("Should not traverse a packed node!")
  } 
}

object SemanticAction {
  
  def convert(t: Any): Any = t match {
    case StarList(s, List()) => ()
    case StarList(s, xs)     => xs
    case PlusList(s, xs)     => xs
    case OptList(s, List())  => ()
    case OptList(s, xs)      => xs
    case _                   => t 
  }
  
  def filterUnit(left: Any, right: Any) =
    if (left == () && right == ()) ()
    else if (left == ()) right
    else if (right == ()) left
    else (left, right)
  
  def amb(input: Input)(s: Set[Any], l: Int, r: Int) = throw new RuntimeException
  
  def t(input: Input)(l: Int, r: Int) = ()
      
  def nt(input: Input)(t: RuleType, v: Any, l: Int, r: Int) = 
    if (t.action.isDefined)
      if (v == ()) t.action.get(input.substring(l, r)) else t.action.get(convert(v)) 
    else v
    
  def int(input: Input)(t: RuleType, left: Any, right: Any) = 
    if (t.action.isDefined)
      t.action.get(filterUnit(left, right))
    else filterUnit(left, right)
  
  def execute(node: NonPackedNode)(implicit input: Input) =
    new SemanticActionExecutor(amb(input), t(input), int(input), nt(input)).visit(node)
}

object TreeBuilder {

   def convert(t: Any): Tree = t match {
    case StarList(s, xs) => Appl(RegularRule(Star(s)), xs.asInstanceOf[Seq[Tree]]) 
    case PlusList(s, xs) => Appl(RegularRule(Plus(s)), xs.asInstanceOf[Seq[Tree]])
    case OptList(s, xs)  => Appl(RegularRule(Opt(s)), xs.asInstanceOf[Seq[Tree]])
    case _               => t.asInstanceOf[Tree]
  }
  
  def flatten(t: Any): Seq[Any] = t match {
    case (t: (_, _), y) => flatten(t) :+ convert(y)
    case (x, y) => List(convert(x), convert(y))
    case x      => List(convert(x))
  }
  
  def amb(input: Input)(s: Set[Any], l: Int, r: Int): Tree = Amb(s.asInstanceOf[Set[Tree]])
  
  def t(input: Input)(l: Int, r: Int): Tree = Terminal(input.substring(l, r))
  
  def int(input: Input)(t: RuleType, left: Any, right: Any) = (left, right)
  
  def nt(input: Input)(t: RuleType, v: Any, l: Int, r: Int) = Appl(t, flatten(v).asInstanceOf[Seq[Tree]])

  
  def newBuilder(implicit input: Input): SPPFVisitor = {
    new SemanticActionExecutor(amb(input), t(input), int(input), nt(input))
  }
  
  def newMemoBuilder(implicit input: Input): SPPFVisitor = {
    new SemanticActionExecutor(amb(input), t(input), int(input), nt(input)) with Memoization
  }
  
  def build(node: NonPackedNode, memoized: Boolean = true)(implicit input: Input): Tree =
    if (memoized)
      newMemoBuilder.visit(node).asInstanceOf[Tree]
    else 
      newBuilder.visit(node).asInstanceOf[Tree]
}

class SPPFToDot extends SPPFVisitor {
  type T = Unit
  
  def get: String = sb.toString
  
  import org.meerkat.util.visualization._
  import org.meerkat.util.visualization.Shape._
  import org.meerkat.util.visualization.Style._
  
  val sb = new StringBuilder
  
  def visit(node: SPPFNode): T = 
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
  