/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.sppf

import org.meerkat.tree.Tree
import org.meerkat.tree.Tree._
import org.meerkat.tree._
import scala.collection.breakOut
import org.meerkat.util.Input
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

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
                             int: (Rule, Any) => Any,
                             nt:  (Rule, Any, Int, Int) => Any) extends SPPFVisitor {
 
   type T = Any
  
   def ambiguity(n: NonPackedNode): Any =  
     amb((for (p <- n.children) yield nonterminal(p, n.leftExtent, n.rightExtent)) (breakOut), n.leftExtent, n.rightExtent)
   
   def flatten(p: PackedNode, v: Any, leftExtent: Int, rightExtent: Int) = 
     p.ruleType.head match {
       case Star(s) => StarList(s, flattenStar(v))
       case Plus(s) => PlusList(s, flattenPlus(v))
       case Opt(s)  => OptList(s, flattenOpt(v))
       case _       => nt(p.ruleType, v, leftExtent, rightExtent)
     }
   
   def flattenStar(v: Any): List[Any] = v match {
     case ()                          => List()
     case (StarList(s, xs), r)        => xs :+ r
     case PlusList(s, xs)             => xs
     case StarList(s, xs)             => xs
     case x: Any                      => List(x)
   }
   
   def flattenPlus(v: Any): List[Any] = v match {
     case ()                          => List()
     case (PlusList(s, xs), r)        => xs :+ r
     case PlusList(s, xs)             => xs
     case x: Any                      => List(x)         
   }
   
   def flattenOpt(v: Any): List[Any] = v match {
     case ()                          => List()
     case x: Any                      => List(x)
   }
   
   def intermediate(p: PackedNode, l: Any, r: Any): Any = (l,r) match {
     case (StarList(s, xs), ()) => StarList(s, xs)
     case (StarList(s, xs), r)  => StarList(s, xs :+ r)
     case (PlusList(s, xs), ()) => PlusList(s, xs)
     case (PlusList(s, xs), r)  => PlusList(s, xs :+ r)
     case ((), ())              => ()
     case (l, ())               => int(p.ruleType, l)
     case ((), r)               => int(p.ruleType, r)
     case (l, r)                => int(p.ruleType, (l, r))
   } 
   
   def nonterminal(p: PackedNode, leftExtent: Int, rightExtent: Int): Any = 
      flatten(p, visit(p.leftChild), leftExtent, rightExtent)
  
  def visit(node: SPPFNode): Any = node match {
     
     case t: TerminalNode     => if (t.leftExtent == t.rightExtent) () 
                                 else tn(t.leftExtent, t.rightExtent)
                                 
     case t: LayoutTerminalNode  => if (t.leftExtent == t.rightExtent) () 
                                    else tn(t.leftExtent, t.rightExtent)                            
    
     case n: NonterminalNode  => if (n isAmbiguous) ambiguity(n) 
                                 else nonterminal(n.first, n.leftExtent, n.rightExtent)
                                   
     case i: IntermediateNode => if (i isAmbiguous) ambiguity(i) 
                                 else intermediate(i.first, visit(i.first.leftChild), visit(i.first.rightChild))
     
     case p: PackedNode       => throw new RuntimeException("Should not traverse a packed node!")
  }

}

object SemanticAction {
  
  import org.meerkat.parsers.~
  
  def convert(t: Any): Any = t match {
    case StarList(s, xs)       => convert(xs)
    case PlusList(s, xs)       => convert(xs)
    case OptList(s, xs)        => convert(xs)
    case Seq()                => ()
    case l: Seq[Any]          => l.map { convert(_) }.filter { _ != ()}
    case (x, y: EBNFList)      => convert(x, convert(y))
    case (x: EBNFList, y)      => convert(convert(x), y)
    case ((), ())              => ()
    case (x, y)                => new ~(convert(x), convert(y))
    case _                     => t 
  }
  
  def amb(input: Input)(s: Set[Any], l: Int, r: Int) = throw new RuntimeException("Cannot execute while the grammar is ambiguous.")
  
  def t(input: Input)(l: Int, r: Int) = ()
      
  def nt(input: Input)(t: Rule, v: Any, l: Int, r: Int) = 
    if (t.action.isDefined)
      if (v == ()) t.action.get(input.substring(l, r)) else t.action.get(convert(v)) 
    else convert(v)
    
  def int(input: Input)(t: Rule, v: Any) = 
    if (t.action.isDefined)
      t.action.get(v)
    else v
  
  def execute(node: NonPackedNode)(implicit input: Input) =
    convert(new SemanticActionExecutor(amb(input), t(input), int(input), nt(input)).visit(node))
}

object TreeBuilder {

   def convert(t: Any): Tree = t match {
    case StarList(s, xs) => RuleNode(RegularRule(Star(s)), xs map { convert(_) }) 
    case PlusList(s, xs) => RuleNode(RegularRule(Plus(s)), xs map { convert(_) })
    case OptList(s, xs)  => RuleNode(RegularRule(Opt(s)),  xs map { convert(_) })
    case _               => t.asInstanceOf[Tree]
  }
  
  def flatten(t: Any): Seq[Tree] = t match {
    case (t: (_, _), y)  => flatten(t) :+ convert(y)
    case (x, y)          => List(convert(x), convert(y))
    case ()              => List()
    case x               => List(convert(x))
  }
  
  def flatten(s: Symbol): Seq[Symbol] = s match {
    case Sequence(l @ _*) => l flatMap (flatten(_))
    case _                => ArrayBuffer(s)
  }
  
  def amb(input: Input)(s: Set[Any], l: Int, r: Int): Tree = AmbNode(s.asInstanceOf[Set[Tree]])
  
  def t(input: Input)(l: Int, r: Int): Tree = org.meerkat.tree.TerminalNode(input.substring(l, r))
  
  def int(input: Input)(t: Rule, v: Any) = v
  
  def nt(input: Input)(t: Rule, v: Any, l: Int, r: Int) = RuleNode(Rule(t.head, flatten(t.body)), flatten(v).asInstanceOf[Seq[Tree]])
  
  def build(node: NonPackedNode, memoized: Boolean = true)(implicit input: Input): Tree = {
    val executer = if (memoized) new SemanticActionExecutor(amb(input), t(input), int(input), nt(input)) with Memoization
                   else new SemanticActionExecutor(amb(input), t(input), int(input), nt(input)) 
    
    convert(executer.visit(node))               
  }
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
  