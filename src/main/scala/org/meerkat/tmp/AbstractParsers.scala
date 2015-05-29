package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import scala.reflect.ClassTag
import org.meerkat.sppf.Slot
import org.meerkat.tree.RuleType

trait MonadPlus[+T, M[+F] <: MonadPlus[F,M]] {
  def map[U](f: T => U)(implicit m: Memoizable[T]): M[U]
  def flatMap[U](f: T => M[U])(implicit m: Memoizable[T]): M[U]
  def orElse[U >: T](r: => M[U]): M[U]
  def filter(pred: T => Boolean): M[T]
  
  // Specialization only for optimization purposes
  def smap[U](f: T => U): M[U]
}

case class ~[+A,+B](_1: A, _2: B)

trait AbstractParsers {
  
  type Result[+T] <: MonadPlus[T, Result]
  
  trait AbstractParser[+T] extends ((Input, Int, SPPFLookup) => Result[T]) { def symbol: org.meerkat.tree.Symbol }
  
  type AbstractSequence[+T] = AbstractParser[T] with Slot { 
    def size: Int
    def symbol: org.meerkat.tree.Sequence
    def action(f: Any => Any): Unit
  }
  
  type AbstractAlternation[+T] = AbstractParser[T] { def symbol: org.meerkat.tree.Alt }
  
  type AbstractSymbol[+T] = AbstractParser[T] { type Value; def name: String }
  
  type AbstractNonterminal[+T] = AbstractSymbol[T] { def symbol: org.meerkat.tree.Nonterminal }
  
  type Head = AbstractNonterminal[Any]
  
  type AbstractSequenceBuilder[+T] = (Slot => AbstractSequence[T]) { type Value }
  type AbstractAlternationBuilder[+T] = (Head => AbstractAlternation[T]) { type Value }
  
  trait CanBuildSequence[A,B,ValA,ValB] {
    
    type T
    type Val
    
    type Sequence <: AbstractSequence[T]
    def sequence(p: AbstractSequence[T]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup): T
    
    type SequenceBuilder <: (Slot => Sequence) { type Value = Val }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }
  
  trait CanBuildAlternation[B,ValB] {
    
    type Alternation <: AbstractAlternation[B]
    def alternation(f: AbstractParser[B]): Alternation
    
    def result(e: B, p: Slot, nt: Head, sppfLookup: SPPFLookup): B
    
    type AlternationBuilder <: (Head => Alternation) { type Value = ValB }
    def builderAlt(f: Head => Alternation): AlternationBuilder
  }
  
  trait CanBuildNonterminal[A,ValA] {
    
    type Nonterminal <: AbstractNonterminal[A] { type Value = ValA }
    def nonterminal(name: String, p: AbstractParser[A]): Nonterminal
  }
  
  trait CanBuildEBNF[A,ValA] {
    type Regular <: AbstractNonterminal[A] { type Value = List[ValA] }   
    type Group <: AbstractNonterminal[A] { type Value = ValA }
    
    def regular(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[A]): Regular
    def group(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[A]): Group
  }
    
  object AbstractParser {
    
    import org.meerkat.tmp.Negation._
    
    def seq[A:Memoizable,B:Memoizable](p1: AbstractSequenceBuilder[A], p2: AbstractSymbol[B])
                                      (implicit builder: CanBuildSequence[A,B,p1.Value,p2.Value]): builder.SequenceBuilder
      = builder builderSeq { slot => val q1 = p1(slot); sequence(slot, q1.size + 1, q1, p2) }
    
    def seq[A:Memoizable,B:Memoizable](p1: AbstractSymbol[A], p2: AbstractSymbol[B])
                                      (implicit builder: CanBuildSequence[A,B,p1.Value,p2.Value]): builder.SequenceBuilder
      = builder builderSeq { slot => sequence(slot, 2, p1, p2) }
    
    protected def sequence[A:Memoizable,B:Memoizable,ValA,ValB](slot: Slot, len: Int, p1: AbstractParser[A], p2: AbstractParser[B])
                                                               (implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.Sequence = {
      import builder._
      builder sequence (new AbstractParser[T] with Slot {
                          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
                            = p1(input, i, sppfLookup) flatMap { x1 => p2(input, index(x1), sppfLookup).smap { x2 => intermediate(x1, x2, this, sppfLookup) } }
                          def size = len
                          def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)
                          def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, len)
                          
                          def action(f: Any => Any) = { println("Semantic action has been added!"); this.ruleType.action = f }
                          override def toString = s"[${ruleType.toString()},$size]"
                        })
    }
    
    def altAlt[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractAlternationBuilder[B])
                        (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                  m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(p1(head), p2(head)) }
    
    def altAltSeq[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractSequenceBuilder[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(p1(head), alt(head, p2)) }
    
    def altSeqAlt[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractAlternationBuilder[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), p2(head)) }
    
    def altAltSym[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractSymbol[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(p1(head), alt(head, p2)) }
    
    def altSymAlt[A,B >: A](p1: AbstractSymbol[A], p2: AbstractAlternationBuilder[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), p2(head)) }
    
    def altSeq[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractSequenceBuilder[B])
                        (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                   m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), alt(head, p2)) }
    
    def altSymSeq[A,B >: A](p1: AbstractSymbol[A], p2: AbstractSequenceBuilder[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), alt(head, p2)) }
    
    def altSeqSym[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractSymbol[B])
                           (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                      m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), alt(head, p2)) }
    
    def altSym[A,B >: A](p1: AbstractSymbol[A], p2: AbstractSymbol[B])
                        (implicit builder1: CanBuildAlternation[A,p1.Value], builder2: CanBuildAlternation[B,p2.Value], 
                                   m1: Memoizable[A], m2: Memoizable[B], sub: p1.Value <:< p2.Value): builder2.AlternationBuilder
      = builder2 builderAlt { head => alternation(alt(head, p1), alt(head, p2)) }
    
    def alt[B:Memoizable,BVal](head: Head, p: AbstractSequenceBuilder[B])(implicit builder: CanBuildAlternation[B,BVal]): AbstractParser[B]
      = new AbstractParser[B] with Slot { 
          val q = p(this)
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = q(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
          def symbol = q.symbol
          def ruleType = org.meerkat.tree.Rule(head.symbol, this.symbol)
          override def toString = s"p${this.hashCode()}"
        }
    
    def alt[B:Memoizable,BVal](head: Head, p: AbstractSymbol[B])(implicit builder: CanBuildAlternation[B,BVal]): AbstractParser[B]
      = new AbstractParser[B] with Slot { 
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
          def symbol = p.symbol
          def ruleType = org.meerkat.tree.Rule(head.symbol, this.symbol)
          override def toString = s"p${this.hashCode()}"
        }
    
    protected def alternation[A, B >: A, BVal](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[B,BVal], m1: Memoizable[A], m2: Memoizable[B]): builder.Alternation
      = builder alternation new AbstractParser[B] {
                            def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p1(input,i,sppfLookup) orElse p2(input,i,sppfLookup)
                            def symbol = org.meerkat.tree.Alt(p1.symbol, p2.symbol)
                          }
  }
  
}
 
object AbstractCPSParsers extends AbstractParsers {
    
  type Result[+T] = CPSResult[T]
  
  import AbstractParser._
  
  protected def memoize[A: Memoizable](p: => AbstractParser[A])(implicit obj: ClassTag[Result[A]]): AbstractParser[A] = {
    lazy val q: AbstractParser[A] = p
    var table: Array[Result[A]] = null
    
    new AbstractParser[A] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = {
        if (table == null) table = new Array(input.length + 1)
        val result = table(i)
        if (result == null) {
          table(i) = CPSResult.memo(q(input,i,sppfLookup))
          table(i)
        } else
          result
      }
      
      def symbol = q.symbol
    }
  }
  
  def nonterminalSym[A:Memoizable](name: String, p: AbstractSymbol[A])
                                  (implicit builder1: CanBuildNonterminal[A,p.Value], builder2: CanBuildAlternation[A,p.Value], 
                                            obj: ClassTag[Result[A]]): builder1.Nonterminal = { import builder1._
    lazy val q: Nonterminal = builder1 nonterminal (name, memoize(alt(q, p))); q
  }
  
  def nonterminalSeq[A:Memoizable,T](name: String, p: => AbstractSequenceBuilder[A])
                                    (implicit builder1: CanBuildNonterminal[A,T], builder2: CanBuildAlternation[A,T], 
                                              obj: ClassTag[Result[A]]): builder1.Nonterminal = { import builder1._
    lazy val q: Nonterminal = builder1 nonterminal (name, memoize(alt(q, p))); q
  }
  
  def nonterminalAlt[A:Memoizable,T](name: String, p: => AbstractAlternationBuilder[A])
                                    (implicit builder: CanBuildNonterminal[A,T], 
                                              obj: ClassTag[Result[A]]): builder.Nonterminal = { import builder._
    lazy val q: Nonterminal = builder nonterminal (name, memoize(p(q))); q
  }
  
  def regular[A:Memoizable,T](symbol: org.meerkat.tree.Nonterminal, p: => AbstractAlternationBuilder[A])
                            (implicit builder: CanBuildEBNF[A,T], obj: ClassTag[Result[A]]): builder.Regular = { import builder._
    lazy val q: Regular = builder.regular(symbol, memoize(p(q))); q
  }
  
}