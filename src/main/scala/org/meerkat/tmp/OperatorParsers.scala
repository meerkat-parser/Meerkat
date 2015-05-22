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
      
      def sequence(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence { def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2) }
      
      def infix(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            override def infix = true
          }
      
      def prefix(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
    	      def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
    			  override def prefix = true
          }
      
      def postfix(p: (Prec, Prec) => obj.Sequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            override def postfix = true
          }
      
      
      def left(p: OperatorSequence): OperatorSequence 
        = if (p.infix) 
            new OperatorSequence {
              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
              override def infix = p.infix
              override def assoc: Assoc.Assoc = Assoc.LEFT
            }
        else p
      
      def right(p: OperatorSequence): OperatorSequence 
        = if (p.infix)
            new OperatorSequence {
              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
              override def infix = p.infix
              override def assoc: Assoc.Assoc = Assoc.RIGHT
            }
          else p
      
      def non_assoc(p: OperatorSequence): OperatorSequence 
        = new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            
            override def infix = p.infix
            override def prefix = p.prefix
            override def postfix = p.postfix            
            override def assoc: Assoc.Assoc = Assoc.NON_ASSOC
          }
    }
    
    implicit object obj2 extends CanBuildAlternation[NonPackedNode, NonPackedNode] {
      implicit val obj1 = Parsers.obj2
      implicit val obj2 = Parsers.obj2
      implicit val m1 = Parsers.obj3
      implicit val m2 = Parsers.obj3
      
      type OperatorAlternation = OperatorParsers.OperatorAlternation
      
      def alternation(p: Prec => AbstractCPSParsers.AbstractParser[NonPackedNode]): OperatorAlternation 
        = new OperatorAlternation { def apply(prec: Prec) = p(prec) }
    }
  
  }
  
  trait OperatorSequenceBuilder extends SequenceBuilder[OperatorSequence] { import OperatorImplicits._
    type T = NonPackedNode; type S = OperatorSequence; type A = OperatorAlternation
    
    def ~ (p: OperatorNonterminal) = builderSeq(AbstractOperatorParser.seq[T,T,S](this, p)(obj1))
    def ~ (p: Symbol) = builderSeq(AbstractOperatorParser.seq[T,T,S](this, p)(obj1))
    
    def | (p: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A,A](AbstractOperatorParser.alt[T,S](this)(obj2), p)(obj2))
    def | (p: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A,A](AbstractOperatorParser.alt[T,S](this)(obj2), AbstractOperatorParser.alt[T,S](p)(obj2))(obj2))
    def | (p: OperatorNonterminal) = builderAlt(AbstractOperatorParser.alt[T,T,A](AbstractOperatorParser.alt[T,S](this)(obj2), p)(obj2))
    def | (p: Symbol) = builderAlt(AbstractOperatorParser.alt[T,T,A](AbstractOperatorParser.alt[T,S](this)(obj2), p)(obj2))
    
    def |> (p: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.greater[T,T,A,A](AbstractOperatorParser.alt[T,S](this)(obj2), p)(obj2))
    def |> (p: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.greater[T,T,A,A](AbstractOperatorParser.alt[T,S](this)(obj2), AbstractOperatorParser.alt[T,S](p)(obj2))(obj2))
  }
    
  def builderSeq(sb: Head => OperatorSequence): OperatorSequenceBuilder 
    = new OperatorSequenceBuilder { def apply(head: Head) = sb(head) }
  
  trait OperatorAlternationBuilder extends AlternationBuilder[OperatorAlternation] { import OperatorImplicits._
    type T = NonPackedNode; type S = OperatorSequence; type A = OperatorAlternation
    
    def | (p: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A,A](this, p)(obj2))
    def | (p: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A,A](this, AbstractOperatorParser.alt[T,S](p)(obj2))(obj2))
    def | (p: OperatorNonterminal) = builderAlt(AbstractOperatorParser.alt[T,T,A](this, p)(obj2))
    def | (p: Symbol) = builderAlt(AbstractOperatorParser.alt[T,T,A](this, p)(obj2))
    
    def |> (p: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.greater[T,T,A,A](this, p)(obj2))
    def |> (p: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.greater[T,T,A,A](this, AbstractOperatorParser.alt[T,S](p)(obj2))(obj2))
    
  }
  
  def builderAlt(ab: AlternationBuilder[OperatorAlternation]): OperatorAlternationBuilder
    = new OperatorAlternationBuilder { def apply(head: Head, group: Group) = ab(head, group) }
  
  trait OperatorSequence extends AbstractOperatorSequence[NonPackedNode]
  
  trait OperatorAlternation extends AbstractOperatorParser[NonPackedNode]
  
  trait OperatorNonterminal extends AbstractOperatorParser[NonPackedNode] { import OperatorImplicits._
    type T = NonPackedNode; type S = OperatorSequence; type A = OperatorAlternation
    
    def ~ (p: OperatorNonterminal): OperatorSequenceBuilder = builderSeq(AbstractOperatorParser.seq(this, p)(obj1))
    def ~ (p: Symbol): OperatorSequenceBuilder = builderSeq(AbstractOperatorParser.seq(this, p)(obj1))
    
    def | (p: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.alt(this, p)(obj2))
    def | (p: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A](this, AbstractOperatorParser.alt[T,S](p)(obj2))(obj2))
    def | (p: OperatorNonterminal) = AbstractOperatorParser.alt(this, p)(obj2)
    def | (p: Symbol) = AbstractOperatorParser.alt(this, p)(obj2)
  }
  
  implicit class ParsersSeqOps(p: Symbol) { import OperatorImplicits._
    def ~ (q: OperatorNonterminal): OperatorSequenceBuilder = builderSeq(AbstractOperatorParser.seq(p, q)(obj1)) 
  }
  
  implicit class ParsersAltOps(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) { import OperatorImplicits._
    type T = NonPackedNode; type S = OperatorSequence; type A = OperatorAlternation
    
    def | (q: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.alt(p, q)(obj2))
    def | (q: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A](p, AbstractOperatorParser.alt[T,S](q)(obj2))(obj2))
    def | (q: OperatorNonterminal) = AbstractOperatorParser.alt(p, q)(obj2)
  }
  
  implicit class StringSeqOps(term: String) { import OperatorImplicits._
    def ~ (q: OperatorNonterminal) = builderSeq(AbstractOperatorParser.seq(term, q)(obj1)) 
  }
  
  implicit class StringAltOps(term: String) { import OperatorImplicits._
    type T = NonPackedNode; type S = OperatorSequence; type A = OperatorAlternation
    
    val p: AbstractCPSParsers.AbstractParser[NonPackedNode] = term
    
    def | (q: OperatorAlternationBuilder) = builderAlt(AbstractOperatorParser.alt(p, q)(obj2))
    def | (q: OperatorSequenceBuilder) = builderAlt(AbstractOperatorParser.alt[T,T,A](p, AbstractOperatorParser.alt[T,S](q)(obj2))(obj2))
    def | (q: OperatorNonterminal) = AbstractOperatorParser.alt(p, q)(obj2)
  }
  
  def left(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
    builderSeq(head => obj1.left(p(head)))
  }
  
  def right(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
    builderSeq(head => obj1.right(p(head)))
  }
  
  def non_assoc(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
    builderSeq(head => obj1.right(p(head)))
  }
  
  def left(p: OperatorAlternationBuilder): OperatorAlternationBuilder = { import OperatorImplicits._
    type T = NonPackedNode; type A = OperatorAlternation
    builderAlt(AbstractOperatorParser.left[T,A](p)(obj2))
  }
  
  def right(p: OperatorAlternationBuilder): OperatorAlternationBuilder = { import OperatorImplicits._
    type T = NonPackedNode; type A = OperatorAlternation
    builderAlt(AbstractOperatorParser.right[T,A](p)(obj2))
  }
  
  def op_nt(name: String)(p: => OperatorAlternationBuilder) 
    = new OperatorNonterminal { import Parsers._
          val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
          lazy val parser: OperatorAlternation = { val (f,opened,closed) = p(this,Group()); f(opened.close) }
      
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