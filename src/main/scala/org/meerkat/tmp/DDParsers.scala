package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup

case class ~[+A, +B](_1: A, _2: B)

object DDParsers extends AbstractParsers {
  
  type Result[+T] = CPSResult[T]
  
  implicit def obj1[A, B] = new Composable[(NonPackedNode, A), (NonPackedNode, B)] {
    type R = (NonPackedNode, A~B)
    
    type Sequence = DDParsers.Sequence[A~B]
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[(NonPackedNode, A~B)]): Sequence 
      = new DDParsers.Sequence[A~B] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: (NonPackedNode, A)): Int = a._1.rightExtent
    def intermediate(a: (NonPackedNode, A), b: (NonPackedNode, B), p: AbstractParser[(NonPackedNode, A~B)], sppfLookup: SPPFLookup): (NonPackedNode, A~B) 
      = (sppfLookup.getIntermediateNode(p, a._1, b._1), new ~(a._2, b._2)) 
  }
  
  implicit def obj2[A, B >: A] = new Alternative[(NonPackedNode, A), (NonPackedNode, B)] {
    type Alternation = DDParsers.Alternation[B]
    
    def alternation(f: (Input, Int, SPPFLookup) => Result[(NonPackedNode, B)]): Alternation
      = new DDParsers.Alternation[B] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) }
    
    def result(e: (NonPackedNode, B), p: AbstractParser[(NonPackedNode, B)], nt: AbstractParser[Any], sppfLookup: SPPFLookup): (NonPackedNode, B)
      = (sppfLookup.getNonterminalNode(nt, p, e._1), e._2)
  }
  
  implicit def obj3[A] = new Memoizable[(NonPackedNode, A)] {
    type U = (Int, A)
    def value(t: (NonPackedNode, A)): (Int, A) = (t._1.rightExtent, t._2)
  }
  
  trait Sequence[+T] extends AbstractParser[(NonPackedNode, T)] { 
    override def isSequence = true
    
    def ~ [U](p: Symbol[U]): Sequence[T~U] = AbstractParser.seq(this, p)
  }
  
  trait Alternation[+T] extends AbstractParser[(NonPackedNode, T)] { 
    override def isAlternation = true
    
    def | [U >: T](p: Sequence[U]): Alternation[U] = AbstractParser.alt(this, p)
    def | [U >: T](p: Symbol[U]): Alternation[U] = AbstractParser.alt(this, p)
  }
  
  trait Symbol[+T] extends AbstractParser[(NonPackedNode, T)] {
    def ~ [U](p: Symbol[U]): Sequence[T~U] = AbstractParser.seq(this, p)
    def | [U >: T](p: Symbol[U]): Alternation[U] = AbstractParser.alt(this, p)
  }
  
  trait Nonterminal[+T] extends Symbol[T] { 
    override def isNonterminal = true 
  }
  
  trait Terminal[+T] extends Symbol[T] { 
    override def isTerminal = true 
  }

}