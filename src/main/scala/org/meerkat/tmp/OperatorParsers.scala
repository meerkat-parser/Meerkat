package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import java.util.HashMap
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.Visualization

object OperatorParsers {
  
  import AbstractOperatorParsers._
  import Parsers._
  
  object OperatorImplicits {
  
    implicit object obj1 extends CanBuildSequence[NonPackedNode, NonPackedNode] {
      implicit val obj = Parsers.obj1
      implicit val m1 = Parsers.obj3
      implicit val m2 = Parsers.obj3
      
      type OperatorSequence = OperatorParsers.OperatorSequence
      def sequence(f: AbstractOperatorParser[Any] => OperatorSequence): OperatorSequence 
        = new OperatorSequence {
            lazy val p: OperatorSequence = f(head.get)
            
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            
            override def isInfix = p.isInfix
            override def isPostfix = p.isPostfix
            override def isPrefix = p.isPrefix
            override def assoc = p.assoc
          }
      
      def infixOp(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            override def isInfix = true
          }
      
      def postfixOp(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            override def isPostfix = true
          }
      
      def prefixOp(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            override def isPrefix = true
          }
      
      def sequenceOp(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
          }
      
      def left(p: OperatorSequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            
            override def isInfix = p.isInfix
            override def isPostfix = p.isPostfix
            override def isPrefix = p.isPrefix
            
            override def assoc: Assoc.Assoc = Assoc.LEFT
          }
      
      def right(p: OperatorSequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            
            override def isInfix = p.isInfix
            override def isPostfix = p.isPostfix
            override def isPrefix = p.isPrefix
            
            override def assoc: Assoc.Assoc = Assoc.RIGHT
          }
      
      def non_assoc(p: OperatorSequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            
            override def isInfix = p.isInfix
            override def isPostfix = p.isPostfix
            override def isPrefix = p.isPrefix
            
            override def assoc: Assoc.Assoc = Assoc.NON_ASSOC
          }
    }
    
    implicit object obj2 extends CanBuildAlternation[NonPackedNode, NonPackedNode] {
      implicit val obj1 = Parsers.obj2
      implicit val obj2 = Parsers.obj2
      implicit val m1 = Parsers.obj3
      implicit val m2 = Parsers.obj3
      
      type OperatorAlternation = OperatorParsers.OperatorAlternation
      def alternation(f: (AbstractOperatorParser[Any], Group) => Prec => AbstractCPSParsers.AbstractParser[NonPackedNode]): OperatorAlternation 
        = new OperatorAlternation {
            lazy val p: Prec => AbstractCPSParsers.AbstractParser[NonPackedNode] = f(head.getOrElse(this), group.getOrElse(Group()))
            def apply(prec: Prec) = p(prec)
          }
    }
  
  }
  
  trait OperatorSequence extends AbstractOperatorSequence[NonPackedNode] { import OperatorImplicits._
    
    private var hd: Option[AbstractOperatorParser[Any]] = None
    override def head = hd
    override def pass(head: AbstractOperatorParser[Any]) = hd = Option(head)
    
    def ~ (p: OperatorNonterminal): OperatorSequence = AbstractOperatorParser.seq(this, p)(obj1)
    def ~ (p: Symbol): OperatorSequence = AbstractOperatorParser.seq(this, p)(obj1)
    
    def | (p: OperatorSequence): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)   
    def | (p: OperatorNonterminal): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    def | (p: OperatorAlternation): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)

    def | (p: Sequence): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    def | (p: Symbol): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    
    def |> (p: OperatorSequence): OperatorAlternation = AbstractOperatorParser.greater(this, p)(obj2)
    // def |> (p: OperatorAlternation): OperatorAlternation = AbstractOperatorParser.greater(this, p)(obj2)
  }
  
  trait OperatorParserWithAlternationOp extends AbstractOperatorParser[NonPackedNode] { import OperatorImplicits._
    
    private var hd: Option[AbstractOperatorParser[Any]] = None
    override def head = hd
    
    private var gr: Option[Group] = None
    override def group = gr
    
    override def pass(head: AbstractOperatorParser[Any], group: Group) = { hd = Option(head); gr = Option(group) }
    
    def | (p: OperatorSequence): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)   
    def | (p: OperatorNonterminal): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    def | (p: OperatorAlternation): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)

    def | (p: Sequence): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    def | (p: Symbol): OperatorAlternation = AbstractOperatorParser.alt(this, p)(obj2)
    
    def |> (p: OperatorSequence): OperatorAlternation = AbstractOperatorParser.greater(this, p)(obj2)
  }
  
  trait OperatorAlternation extends OperatorParserWithAlternationOp
  
  trait OperatorNonterminal extends OperatorParserWithAlternationOp { import OperatorImplicits._
    def ~ (p: OperatorNonterminal): OperatorSequence = AbstractOperatorParser.seq(this, p)(obj1)
    def ~ (p: Symbol): OperatorSequence = AbstractOperatorParser.seq(this, p)(obj1)
  }
  
  implicit class ParsersSeqOps(p: Symbol) { import OperatorImplicits._
    def ~ (q: OperatorNonterminal) = AbstractOperatorParser.seq(p, q)(obj1) 
  }
  
  implicit class ParsersAltOps(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) { import OperatorImplicits._
    def | (q: OperatorSequence) = AbstractOperatorParser.alt(p, q)(obj2)
    def | (q: OperatorNonterminal) = AbstractOperatorParser.alt(p, q)(obj2)
    def | (q: OperatorAlternation) = AbstractOperatorParser.alt(p, q)(obj2)
  }
  
  implicit class StringSeqOps(term: String) { import OperatorImplicits._
    def ~ (q: OperatorNonterminal) = AbstractOperatorParser.seq(term, q)(obj1) 
  }
  
  implicit class StringAltOps(term: String) { import OperatorImplicits._
    val p: AbstractCPSParsers.AbstractParser[NonPackedNode] = term
    def | (q: OperatorSequence) = AbstractOperatorParser.alt(p, q)(obj2)
    def | (q: OperatorNonterminal) = AbstractOperatorParser.alt(p, q)(obj2)
  }
  
  def left(p: OperatorSequence): OperatorSequence = { import OperatorImplicits._
    AbstractOperatorParser.left(obj1)(p)
  }
  
  def right(p: OperatorSequence): OperatorSequence = { import OperatorImplicits._
    AbstractOperatorParser.right(obj1)(p)
  }
  
  def non_assoc(p: OperatorSequence): OperatorSequence = { import OperatorImplicits._
    AbstractOperatorParser.non_assoc(obj1)(p)
  }
  
  def op_nt(name: String)(p: => AbstractOperatorParser[NonPackedNode])
    = new OperatorNonterminal { import Parsers._
        val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
        lazy val parser: AbstractOperatorParser[NonPackedNode] = { val q = p; q pass (this, Group()); q }
      
        def apply(prec: Prec) 
          = if (table.containsKey(prec)) table.get(prec) 
            else { 
              val nt = AbstractCPSParsers.memoize(parser(prec), name + s"$prec")
              table.put(prec, nt)
              nt
            }
        
        override def toString = name
        
      }
  
  def run(input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[NonPackedNode]): Unit = {
    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
    Trampoline.run
  }
  
  def parse(sentence: String, parser: OperatorNonterminal): Unit = {
    val input = new Input(sentence)
    val sppf = new DefaultSPPFLookup(input)
    
    val p = parser((0,0))
    run(input, sppf, p)
    
    println(s"Trying to find: ${p.name}(0,${sentence.length()})")
    val startSymbol = sppf.getStartNode(p, 0, sentence.length())
    
    startSymbol match {
      case None       => println("Parse error")
      case Some(node) => println("Success: " + node)
                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                         println("Visualizing...") 
                         Visualization.visualize(Visualization.toDot(startSymbol.get), "sppf")
                         println("Done!")
    }
  }
  
}