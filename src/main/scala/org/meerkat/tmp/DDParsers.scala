//package org.meerkat.tmp
//
//import org.meerkat.sppf.NonPackedNode
//import org.meerkat.util.Input
//import org.meerkat.sppf.SPPFLookup
//import org.meerkat.sppf.Slot
//
//object DDParsers {
//  
//  import AbstractCPSParsers._
//  import org.meerkat.tmp.~
//  
//  implicit def obj1[A,B] = new CanBuildSequence[(NonPackedNode,A), (NonPackedNode,B)] {
//    
//    type T = (NonPackedNode,A~B)
//    type Sequence = DDParsers.Sequence[A~B]
//    
//    def sequence(p: AbstractSequence[T]): Sequence 
//      = new Sequence { 
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol
//          def ruleType = p.ruleType
//          def size = p.size
//        }
//    
//    def index(a: (NonPackedNode, A)): Int = a._1.rightExtent
//    def intermediate(a: (NonPackedNode,A), b: (NonPackedNode,B), p: Slot, sppfLookup: SPPFLookup): T 
//      = (sppfLookup.getIntermediateNode(p, a._1, b._1), new ~(a._2, b._2))
//      
//    type SequenceBuilder = DDParsers.SequenceBuilder[A~B]
//    def builderSeq(f: Slot => Sequence): SequenceBuilder = new SequenceBuilder { def apply(slot: Slot) = f(slot) }
//  }
//  
//  implicit def obj2[A] = new CanBuildAlternation[(NonPackedNode,A)] {
//    
//    type Alternation = DDParsers.Alternation[A] 
//    def alternation(p: AbstractParser[(NonPackedNode,A)]): Alternation
//      = new Alternation {
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
//        }
//    
//    def result(e: (NonPackedNode,A), p: Slot, nt: Head, sppfLookup: SPPFLookup): (NonPackedNode,A) = (sppfLookup.getNonterminalNode(nt,p,e._1),e._2)
//    
//    type AlternationBuilder = DDParsers.AlternationBuilder[A]
//    def builderAlt(f: Head => Alternation): AlternationBuilder = new AlternationBuilder { def apply(head: Head) = f(head) }
//  }
//  
//  implicit def obj3[A] = new Memoizable[(NonPackedNode,A)] {
//    type U = (Int, A)
//    def value(t: (NonPackedNode, A)): (Int, A) = (t._1.rightExtent, t._2)
//  }
//  
//  implicit def obj4[A] = new CanBuildNonterminal[(NonPackedNode, A)] {
//    type Nonterminal = DDParsers.Nonterminal[A]
//    def nonterminal(nt: String, p: AbstractParser[(NonPackedNode,A)]): Nonterminal 
//      = new Nonterminal {
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = org.meerkat.tree.Nonterminal(nt)
//          def name = nt
//          override def toString = name
//        }
//  }
//  
//  implicit def obj5[B] = new CanBuildSequence[NonPackedNode,(NonPackedNode,B)] {
//    
//    type T = (NonPackedNode,B)  
//    type Sequence = DDParsers.Sequence[B]
//    
//    def sequence(p: AbstractSequence[T]): Sequence 
//      = new Sequence { 
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol
//          def ruleType = p.ruleType
//          def size = p.size
//        }
//    
//    def index(a: NonPackedNode): Int = a.rightExtent
//    def intermediate(a: NonPackedNode, b: (NonPackedNode,B), p: Slot, sppfLookup: SPPFLookup): T 
//      = (sppfLookup.getIntermediateNode(p, a, b._1), b._2)
//      
//    type SequenceBuilder = DDParsers.SequenceBuilder[B]
//    def builderSeq(f: Slot => Sequence): SequenceBuilder = new SequenceBuilder { def apply(slot: Slot) = f(slot) }
//  }
//  
//  implicit def obj6[A] = new CanBuildSequence[(NonPackedNode,A),NonPackedNode] {
//    
//    type T = (NonPackedNode,A)
//    type Sequence = DDParsers.Sequence[A]
//    
//    def sequence(p: AbstractSequence[T]): Sequence 
//      = new Sequence { 
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol
//          def ruleType = p.ruleType
//          def size = p.size
//        }
//    
//    def index(a: (NonPackedNode,A)): Int = a._1.rightExtent
//    def intermediate(a: (NonPackedNode,A), b: NonPackedNode, p: Slot, sppfLookup: SPPFLookup): T 
//      = (sppfLookup.getIntermediateNode(p, a._1, b), a._2)
//      
//    type SequenceBuilder = DDParsers.SequenceBuilder[A]
//    def builderSeq(f: Slot => Sequence): SequenceBuilder = new SequenceBuilder { def apply(slot: Slot) = f(slot) }
//  }
//  
//  trait Sequence[+T] extends AbstractParser[(NonPackedNode,T)] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
//  
//  trait Alternation[+T] extends AbstractParser[(NonPackedNode,T)] { def symbol: org.meerkat.tree.Alt }
//  
//  trait Nonterminal[+T] extends AbstractParser[(NonPackedNode,T)] { import AbstractParser._ 
//    def name: String
//    def symbol: org.meerkat.tree.Nonterminal
//    
//    def ~ [U](p: Nonterminal[U]): SequenceBuilder[T~U] = seq(this, p)
//    
//    implicit val m = Parsers.obj3
//    implicit val obj = Parsers.obj2
//    
//    def ~ [U](p: Parsers.Symbol): SequenceBuilder[T] = seq(this, p)
//    
//    def | [U >: T](p: AlternationBuilder[U]): AlternationBuilder[U] = altSymAlt(this, p)
//    def | [U >: T](p: SequenceBuilder[U]): AlternationBuilder[U] = altSymSeq(this, p)
//    def | [U >: T](p: Nonterminal[U]): AlternationBuilder[U] = altSym(this, p)    
//  }
//  
//  trait SequenceBuilder[+T] extends (Slot => Sequence[T]) { import AbstractParser._
//    
//    def ~ [U](p: Nonterminal[U]): SequenceBuilder[T~U] = seq(this, p)
//    
//    implicit val m = Parsers.obj3
//    implicit val obj = Parsers.obj2
//    
//    def ~ [U](p: Parsers.Symbol): SequenceBuilder[T] = seq(this, p)
//    
//    def | [U >: T](p: AlternationBuilder[U]): AlternationBuilder[U] = altSeqAlt(this, p)
//    def | [U >: T](p: SequenceBuilder[U]): AlternationBuilder[U] = altSeq(this, p)
//    def | [U >: T](p: Nonterminal[U]): AlternationBuilder[U] = altSeqSym(this, p)
//  }
//  
//  trait AlternationBuilder[+T] extends (Head => Alternation[T]) { import AbstractParser._
//    def | [U >: T](p: AlternationBuilder[U]): AlternationBuilder[U] = altAlt(this, p)
//    def | [U >: T](p: SequenceBuilder[U]): AlternationBuilder[U] = altAltSeq(this, p)
//    def | [U >: T](p: Nonterminal[U]): AlternationBuilder[U] = altAltSym(this, p)
//  }
//
//  implicit class ParsersSequenceOps(p: Parsers.SequenceBuilder) { import AbstractParser._
//    implicit val m = Parsers.obj3
//    implicit val obj = Parsers.obj2
//    def ~ [U] (q: Nonterminal[U]): SequenceBuilder[U] = seq(p, q)
//  }
//  
//  implicit class ParsersSymbolOps(p: Parsers.Symbol) { import AbstractParser._
//    implicit val m = Parsers.obj3
//    implicit val obj = Parsers.obj2
//    def ~ [U] (q: Nonterminal[U]): SequenceBuilder[U] = seq(p, q)
//  }
//
//  def ntAlt[T](name: String, p: => AlternationBuilder[T]): Nonterminal[T] = nonterminalAlt(name, p)
//  def ntSeq[T](name: String, p: => SequenceBuilder[T]): Nonterminal[T] = nonterminalSeq(name, p)
//  def ntSym[T](name: String, p: Nonterminal[T]): Nonterminal[T] = nonterminalSym(name, p)
//  
//}