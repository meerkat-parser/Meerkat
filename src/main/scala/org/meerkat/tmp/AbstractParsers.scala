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

trait AbstractParsers {
  
  type Result[+T] <: MonadPlus[T, Result]
  
  trait AbstractParser[+T] extends ((Input, Int, SPPFLookup) => Result[T]) { def symbol: org.meerkat.tree.Symbol }
  
  type AbstractSequence[+T] = AbstractParser[T] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
  
  type AbstractAlternation[+T] = AbstractParser[T] { def symbol: org.meerkat.tree.Alt }
  
  type AbstractSymbol[+T] = AbstractParser[T] { type Value; def name: String; def action: Option[Any => Any] }
  
  type AbstractNonterminal[+T] = AbstractSymbol[T] { def symbol: org.meerkat.tree.Nonterminal }
  
  type Head = AbstractNonterminal[Any]
  
  type AbstractSequenceBuilder[+T] = (Slot => AbstractSequence[T]) { type Value; def action: Option[Any => Any] }
  type AbstractAlternationBuilder[+T] = (Head => AbstractAlternation[T]) { type Value }
    
  trait CanBuildSequence[A,B,ValA,ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    type T; type V
    
    type Sequence <: AbstractSequence[T]
    def sequence(p: AbstractSequence[T]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup): T
    
    type SequenceBuilder <: (Slot => Sequence) { type Value = V }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }
  
  trait CanBuildAlternative[A] {
    implicit val m: Memoizable[A]
    def result(e: A, p: Slot, nt: Head, sppfLookup: SPPFLookup): A
  }
  
  trait CanBuildAlternation[A,B,ValA,ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    implicit val o1: CanBuildAlternative[A]
    implicit val o2: CanBuildAlternative[B]
    
    type Alternation <: AbstractAlternation[B]
    def alternation(f: AbstractParser[B]): Alternation
    
    type AlternationBuilder <: (Head => Alternation) { type Value = ValB }
    def builderAlt(f: Head => Alternation): AlternationBuilder
  }
  
  trait CanBuildNonterminal[A,ValA] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[A] { type Value = ValA }
    def nonterminal(name: String, p: AbstractParser[A]): Nonterminal
    
    type Symbol <: AbstractSymbol[A] { type Value = ValA}
    def symbol(p: AbstractSymbol[A]): Symbol
  }
  
  trait CanBuildEBNF[A,ValA] {
    implicit val m: Memoizable[A]
    
    type Regular <: AbstractNonterminal[A] { type Value = ValA }   
    type Group <: AbstractNonterminal[A] { type Value = ValA }
    
    def regular(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[A]): Regular
    def group(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[A]): Group
  }
    
  object AbstractParser {
    
    def seq[A,B](p1: AbstractSequenceBuilder[A], p2: AbstractSymbol[B])(implicit builder: CanBuildSequence[A,B,p1.Value,p2.Value]): builder.SequenceBuilder
      = builder builderSeq { slot => val q1 = p1(slot); sequence(slot,q1,p2,q1.size + 1) }
    
    def seq[A,B](p1: AbstractSymbol[A], p2: AbstractSymbol[B])(implicit builder: CanBuildSequence[A,B,p1.Value,p2.Value]): builder.SequenceBuilder
      = builder builderSeq { slot => sequence(slot,p1,p2,2) }
    
    protected def sequence[A,B,ValA,ValB](slot: Slot, p1: AbstractParser[A], p2: AbstractParser[B], s: Int)
                                         (implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.Sequence = { import builder._
      builder sequence (new AbstractParser[T] with Slot {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
          = p1(input, i, sppfLookup) flatMap { x1 => p2(input, index(x1), sppfLookup).smap { x2 => intermediate(x1, x2, this, sppfLookup) } }
        def size = s; def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)
        def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, s)
        override def toString = s"[${ruleType.toString()},$size]"
      })
    }
    
    def altAlt[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractAlternationBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder
      = builder builderAlt { head => alternation(p1(head), p2(head)) }
    
    def altAltSeq[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractSequenceBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(p1(head), alt(head, p2)) }
    }
    
    def altSeqAlt[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractAlternationBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), p2(head)) }
    }
    
    def altAltSym[A,B >: A](p1: AbstractAlternationBuilder[A], p2: AbstractSymbol[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(p1(head), alt(head, p2)) }
    }
    
    def altSymAlt[A,B >: A](p1: AbstractSymbol[A], p2: AbstractAlternationBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), p2(head)) }
    }
    
    def altSeq[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractSequenceBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSymSeq[A,B >: A](p1: AbstractSymbol[A], p2: AbstractSequenceBuilder[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSeqSym[A,B >: A](p1: AbstractSequenceBuilder[A], p2: AbstractSymbol[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builder builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSym[A,B >: A](p1: AbstractSymbol[A], p2: AbstractSymbol[B])(implicit builder: CanBuildAlternation[A,B,p1.Value,p2.Value], sub: p1.Value <:< p2.Value): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def alt[B](head: Head, p: AbstractSequenceBuilder[B])(implicit builder: CanBuildAlternative[B]) = { import builder._
      new AbstractParser[B] with Slot { 
          val q = p(this)
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = q(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
          def symbol = q.symbol
          val ruletype = org.meerkat.tree.Rule(head.symbol, this.symbol); ruletype.action = p.action
          def ruleType = ruletype 
          override def toString = s"p${this.hashCode}"
        }
    }
    
    def alt[B](head: Head, p: AbstractSymbol[B])(implicit builder: CanBuildAlternative[B]) = { import builder._
      new AbstractParser[B] with Slot { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
        def symbol = p.symbol
        val ruletype = org.meerkat.tree.Rule(head.symbol, this.symbol); ruletype.action = p.action
        def ruleType = ruletype
        override def toString = s"p${this.hashCode}"
      }
    }
    
    protected def alternation[A,B >: A,ValA,ValB](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.Alternation
      = builder alternation new AbstractParser[B] {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p1(input,i,sppfLookup) orElse p2(input,i,sppfLookup)
          def symbol = org.meerkat.tree.Alt(p1.symbol, p2.symbol)
        }
  }
}
 
object AbstractCPSParsers extends AbstractParsers {  import AbstractParser._
    
  type Result[+T] = CPSResult[T]
  
  def nonterminalSym[A](name: String, p: AbstractSymbol[A])(implicit builder: CanBuildNonterminal[A,p.Value], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = nonterminal (name, memoize(alt(q, p))); q
  }
  
  def nonterminalSeq[A,T](name: String, p: => AbstractSequenceBuilder[A])(implicit builder: CanBuildNonterminal[A,T], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = nonterminal (name, memoize(alt(q, p))); q
  }
  
  def nonterminalAlt[A,T](name: String, p: => AbstractAlternationBuilder[A])(implicit builder: CanBuildNonterminal[A,T], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = builder nonterminal (name, memoize(p(q))); q
  }
  
  def regular[A,T](symbol: org.meerkat.tree.Nonterminal, p: => AbstractAlternationBuilder[A])(implicit builder: CanBuildEBNF[A,T], obj: ClassTag[Result[A]]): builder.Regular = { 
    import builder._
    lazy val q: Regular = builder.regular(symbol, memoize(p(q))); q
  }
  
  def preFilter[B](p: AbstractSymbol[B], pred: (Input,Int) => Boolean, prefix: String)(implicit builder: CanBuildNonterminal[B,p.Value]): builder.Symbol = {
      builder symbol new AbstractParser[B] {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = if (pred(input,i)) p(input,i,sppfLookup) else CPSResult.failure[B]
        def name = prefix + " " + p.name; def symbol = p.symbol; override def toString = name
        type Value = p.Value
        def action = None
      }
    }
  
  def postFilter[B](p: AbstractSymbol[B], pred: (Input,B) => Boolean, postfix: String)(implicit builder: CanBuildNonterminal[B,p.Value]): builder.Symbol = {
      builder symbol new AbstractParser[B] {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup) filter { pred(input,_) }
        def name = p.name + " " + postfix; def symbol = p.symbol; override def toString = name
        type Value = p.Value
        def action = None
      }
    }
  
  import CPSResult.memo
  
  protected def memoize[A: Memoizable](p: => AbstractParser[A])(implicit obj: ClassTag[Result[A]]): AbstractParser[A] = {
    lazy val q: AbstractParser[A] = p
    var results: Array[Result[A]] = null
    
    new AbstractParser[A] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = {
        if (results == null) results = new Array(input.length + 1)
        val result = results(i)
        if (result == null) { results(i) = memo(q(input,i,sppfLookup)); results(i) } 
        else result
      }   
      def symbol = q.symbol
    }
  }
  
}