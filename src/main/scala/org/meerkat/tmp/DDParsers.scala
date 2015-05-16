package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup

object DDParsers {
  
  import AbstractCPSParsers._
  
  case class ~[+A, +B](_1: A, _2: B)
  
  implicit def obj1[A, B] = new CanBuildSequence[(NonPackedNode, A), (NonPackedNode, B)] {
    type R = (NonPackedNode, A~B)
    
    type Sequence = DDParsers.Sequence[A~B]
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[(NonPackedNode, A~B)]): Sequence 
      = new DDParsers.Sequence[A~B] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: (NonPackedNode, A)): Int = a._1.rightExtent
    def intermediate(a: (NonPackedNode, A), b: (NonPackedNode, B), p: AbstractParser[(NonPackedNode, A~B)], sppfLookup: SPPFLookup): (NonPackedNode, A~B) 
      = (sppfLookup.getIntermediateNode(p, a._1, b._1), new ~(a._2, b._2)) 
  }
  
  implicit def obj2[A] = new CanBuildAlternation[(NonPackedNode, A)] {
    type Alternation = DDParsers.Alternation[A]
    
    def alternation(f: AbstractParser[Any] => (Input, Int, SPPFLookup) => Result[(NonPackedNode, A)]): Alternation
      = new Alternation { 
          lazy val p: (Input, Int, SPPFLookup) => Result[(NonPackedNode, A)] = f(head.getOrElse(this))
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) 
        }
    
    def result(e: (NonPackedNode, A), p: AbstractParser[(NonPackedNode, A)], nt: AbstractParser[Any], sppfLookup: SPPFLookup): (NonPackedNode, A)
      = (sppfLookup.getNonterminalNode(nt, p, e._1), e._2)
  }
  
  implicit def obj3[A] = new Memoizable[(NonPackedNode, A)] {
    type U = (Int, A)
    def value(t: (NonPackedNode, A)): (Int, A) = (t._1.rightExtent, t._2)
  }
  
  implicit def obj4[A] = new CanBuildNonterminal[(NonPackedNode, A)] {
    type Nonterminal = DDParsers.Nonterminal[A]
    def nonterminal(nt: String, f: AbstractParser[Any] => (Input, Int, SPPFLookup) => Result[(NonPackedNode, A)]): Nonterminal
      = new Nonterminal {
          lazy val p: (Input, Int, SPPFLookup) => Result[(NonPackedNode, A)] = f(this)
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          
          override def name = nt
        }
  }
  
  trait HasSequenceOp[+T] extends AbstractParser[(NonPackedNode, T)] {
    def ~ [U](p: Symbol[U]): Sequence[T~U] = AbstractParser.seq(this, p)
  }
  
  trait HasAlternationOp[+T] extends AbstractParser[(NonPackedNode, T)] {
    def | [U >: T](p: Sequence[U]): Alternation[U] = AbstractParser.alt(this, p)
    def | [U >: T](p: Symbol[U]): Alternation[U] = AbstractParser.alt(this, p)
  }
  
  trait Sequence[+T] extends HasSequenceOp[T] with HasAlternationOp[T] { 
    override def isSequence = true
    
    private val nm = s"p${this.hashCode()}"
    override def name = nm
  }
  
  trait Alternation[+T] extends HasAlternationOp[T] { 
    override def isAlternation = true
    
    private val nm = s"p${this.hashCode()}"
    override def name = nm
    
    private var hd: Option[AbstractParser[Any]] = None
    override def head: Option[AbstractParser[Any]] = hd
    override def pass(head: AbstractParser[Any]) = hd = Option(head)
  }
  
  trait Symbol[+T] extends HasSequenceOp[T] with HasAlternationOp[T] {
    override def isSymbol = true    
  } 
  
  trait Nonterminal[+T] extends Symbol[T] { 
    override def isNonterminal = true 
  }
    
  def nt[T](name: String)(p: => AbstractParser[(NonPackedNode, T)]): Nonterminal[T]
    = memoize(p, name)
  
  implicit def obj5[B] = new CanBuildSequence[NonPackedNode, (NonPackedNode, B)] {
    type R = (NonPackedNode, B)
      
    type Sequence = DDParsers.Sequence[B]
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[(NonPackedNode, B)]): Sequence
      = new DDParsers.Sequence[B] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: NonPackedNode): Int = a.rightExtent
    def intermediate(a: NonPackedNode, b: (NonPackedNode, B), p: AbstractParser[(NonPackedNode, B)], sppfLookup: SPPFLookup): (NonPackedNode, B) 
      = (sppfLookup.getIntermediateNode(p, a, b._1), b._2)
  }
    
  implicit class ParsersSequenceOps(p: Parsers.Sequence) {
    def ~ [U] (q: Symbol[U]): Sequence[U] = AbstractParser.seq(p, q)(Parsers.obj3, obj3, obj5)
  }
  
  implicit class ParsersSymbolOps(p: Parsers.Symbol) {
    def ~ [U] (q: Symbol[U]): Sequence[U] = AbstractParser.seq(p, q)(Parsers.obj3, obj3, obj5)
  }
  
  implicit def obj6[A] = new CanBuildSequence[(NonPackedNode, A), NonPackedNode] {
    type R = (NonPackedNode, A)
      
    type Sequence = DDParsers.Sequence[A]
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[(NonPackedNode, A)]): Sequence
      = new DDParsers.Sequence[A] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: (NonPackedNode, A)): Int = a._1.rightExtent
    def intermediate(a: (NonPackedNode, A), b: NonPackedNode, p: AbstractParser[(NonPackedNode, A)], sppfLookup: SPPFLookup): (NonPackedNode, A) 
      = (sppfLookup.getIntermediateNode(p, a._1, b), a._2)
  }
  
  implicit class DDParsersSequenceOps[+T](p: DDParsers.Sequence[T]) {
    def ~ (q: Parsers.Symbol): Sequence[T] = AbstractParser.seq(p, q)(obj3, Parsers.obj3, obj6)
  }
  
  implicit class DDParsersSymbolOps[+T](p: DDParsers.Symbol[T]) {
    def ~ (q: Parsers.Symbol): Sequence[T] = AbstractParser.seq(p, q)(obj3, Parsers.obj3, obj6)
  }
  
}