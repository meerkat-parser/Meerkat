package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup

object Parsers extends AbstractParsers {
  
  type Result[+T] = CPSResult[T]
  
  implicit object obj1 extends Composable[NonPackedNode, NonPackedNode] {
    type R = NonPackedNode
    
    type Sequence = Parsers.Sequence
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[NonPackedNode]): Sequence 
      = new Parsers.Sequence { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: NonPackedNode): Int = a.rightExtent
    def intermediate(a: NonPackedNode, b: NonPackedNode, p: AbstractParser[NonPackedNode], sppfLookup: SPPFLookup): NonPackedNode 
      = sppfLookup.getIntermediateNode(p, a, b) 
  }
  
  implicit object obj2 extends Alternative[NonPackedNode, NonPackedNode] {
    type Alternation = Parsers.Alternation
    
    def alternation(f: (Input, Int, SPPFLookup) => Result[NonPackedNode]): Alternation
      = new Alternation { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) }
    
    def result(e: NonPackedNode, p: AbstractParser[NonPackedNode], nt: AbstractParser[Any], sppfLookup: SPPFLookup): NonPackedNode
      = sppfLookup.getNonterminalNode(nt, p, e)
  }
  
  implicit object obj3 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent
  }
  
  trait Sequence extends AbstractParser[NonPackedNode] { 
    override def isSequence = true
    
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
  }
  
  trait Alternation extends AbstractParser[NonPackedNode] { 
    override def isAlternation = true
    
    def | (p: Sequence): Alternation = AbstractParser.alt(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Symbol extends AbstractParser[NonPackedNode] {
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Nonterminal extends Symbol { 
    override def isNonterminal = true 
  }
  
  trait Terminal extends Symbol { 
    override def isTerminal = true 
  }

}