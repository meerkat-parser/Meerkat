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

package org.meerkat.parsers

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import scala.reflect.ClassTag
import org.meerkat.sppf.Slot
import org.meerkat.tree.NonterminalSymbol
import org.meerkat.tree.TerminalSymbol

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
  
  trait AbstractParser[+T] extends ((Input, Int, SPPFLookup) => Result[T]) { 
    def symbol: org.meerkat.tree.Symbol
    def reset: Unit = {}
  }
  
  type AbstractSequence[+T] = AbstractParser[T] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
  
  type AbstractAlternation[+T] = AbstractParser[T] { def symbol: org.meerkat.tree.Alt }
  
  type AbstractSymbol[+T,+V] = AbstractParser[T] { def name: String; def action: Option[Any => V] }
  
  type AbstractNonterminal[+T,+V] = AbstractSymbol[T,V] { def symbol: org.meerkat.tree.NonterminalSymbol }
  
  type Head = AbstractNonterminal[Any,Any]
  
  type AbstractSequenceBuilder[+T,+V] = (Slot => AbstractSequence[T]) { def action: Option[Any => V] }
  type AbstractAlternationBuilder[+T,+V] = (Head => AbstractAlternation[T]) { def action: Option[Any => V] }
    
  trait CanBuildSequence[A,B,ValA,ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    type T; type V
    
    type Sequence <: AbstractSequence[T]
    def sequence(p: AbstractSequence[T]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup): T
    
    type SequenceBuilder <: (Slot => Sequence) { def action: Option[Any => V] }
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
    
    type AlternationBuilder <: (Head => Alternation) { def action: Option[Any => ValB] }
    def builderAlt(f: Head => Alternation): AlternationBuilder
  }
  
  trait CanBuildNonterminal[A,ValA] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[A,ValA]
    def nonterminal(name: String, p: AbstractParser[A]): Nonterminal
    
    type Symbol <: AbstractSymbol[A,ValA]
    def symbol(p: AbstractSymbol[A,ValA]): Symbol
  }
  
  trait CanBuildLayout[A,ValA <: NoValue] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[A,ValA]
    def layout(name: String, p: AbstractParser[A]): Nonterminal
  }
  
  trait CanBuildEBNF[A,ValA] {
    implicit val m: Memoizable[A]
    
    type Regular <: AbstractNonterminal[A,ValA]  
    type Group <: AbstractNonterminal[A,ValA]
    
    def regular(symbol: org.meerkat.tree.NonterminalSymbol, p: AbstractParser[A]): Regular
    def group(p: AbstractParser[A]): Group
  }
  
  trait CanMap[A,B,Val] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[B,Val]
    def nonterminal(name: String, p: AbstractParser[B]): Nonterminal
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup): B
    
    type Sequence <: AbstractSequence[B]
    def sequence(p: AbstractSequence[B]): Sequence
    
    type SequenceBuilder <: (Slot => Sequence) { def action: Option[Any => Val] }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }
    
  object AbstractParser {
    
    def seq[A,B,ValA,ValB](p1: AbstractSequenceBuilder[A,ValA], p2: AbstractSymbol[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.SequenceBuilder
      = builder builderSeq { slot => val q1 = p1(slot); sequence(slot,q1,p2,q1.size + 1) }
    
    def seq[A,B,ValA,ValB](p1: AbstractSymbol[A,ValA], p2: AbstractSymbol[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.SequenceBuilder
      = builder builderSeq { slot => sequence(slot,p1,p2,2) }
    
    protected def sequence[A,B,ValA,ValB](slot: Slot, p1: AbstractParser[A], p2: AbstractParser[B], s: Int)
                                         (implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.Sequence = { import builder._
      builder sequence (new AbstractParser[T] with Slot {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p1(input,i,sppfLookup) flatMap { x => p2(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
        def size = s; def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)
        def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, s)
        override def reset = { p1.reset; p2.reset }
        override def toString = s"[${ruleType.toString()},$size]"
      })
    }
    
    def altAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractAlternationBuilder[A,ValA], p2: AbstractAlternationBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder
      = builder builderAlt { head => alternation(p1(head), p2(head)) }
    
    def altAltSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractAlternationBuilder[A,ValA], p2: AbstractSequenceBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(p1(head), alt(head, p2)) }
    }
    
    def altSeqAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractSequenceBuilder[A,ValA], p2: AbstractAlternationBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), p2(head)) }
    }
    
    def altAltSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractAlternationBuilder[A,ValA], p2: AbstractSymbol[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(p1(head), alt(head, p2)) }
    }
    
    def altSymAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractSymbol[A,ValA], p2: AbstractAlternationBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), p2(head)) }
    }
    
    def altSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractSequenceBuilder[A,ValA], p2: AbstractSequenceBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSymSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractSymbol[A,ValA], p2: AbstractSequenceBuilder[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSeqSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractSequenceBuilder[A,ValA], p2: AbstractSymbol[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builder builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def altSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractSymbol[A,ValA], p2: AbstractSymbol[B,ValB])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head => this.alternation(alt(head, p1), alt(head, p2)) }
    }
    
    def alt[B,Val](head: Head, p: AbstractSequenceBuilder[B,Val])(implicit builder: CanBuildAlternative[B]) = { import builder._
      new AbstractParser[B] with Slot { 
          val q = p(this)
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = q(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
          def symbol = q.symbol
          lazy val ruletype = { val rule = org.meerkat.tree.Rule(head.symbol, this.symbol); rule.action = p.action; rule }
          def ruleType = ruletype 
          override def reset = q.reset
          override def toString = s"p${this.hashCode}"
        }
    }
    
    def alt[B,Val](head: Head, p: AbstractSymbol[B,Val])(implicit builder: CanBuildAlternative[B]) = { import builder._
      new AbstractParser[B] with Slot { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup).map { x => builder result (x, this, head, sppfLookup) }
        def symbol = p.symbol
        lazy val ruletype = { val rule = org.meerkat.tree.Rule(head.symbol, this.symbol); rule.action = p.action; rule }
        def ruleType = ruletype
        override def reset = p.reset
        override def toString = s"p${this.hashCode}"
      }
    }
    
    protected def alternation[A,B >: A,ValA,ValB](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.Alternation
      = builder alternation new AbstractParser[B] {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p1(input,i,sppfLookup) orElse p2(input,i,sppfLookup)
          def symbol = org.meerkat.tree.Alt(p1.symbol, p2.symbol)
          override def reset = { p1.reset; p2.reset }
        }
    
    def map[A,B,ValA](p: AbstractSequenceBuilder[A,ValA], f: A => B)(implicit builder: CanMap[A,B,ValA]): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[B] with Slot {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = q(input,i,sppfLookup) map f
          def size = q.size; def ruleType = q.ruleType; def symbol = q.symbol
          override def reset = q.reset
        }) }
    }
    
    def map[A,B,ValA](p: AbstractSymbol[A,ValA], f: A => B)(implicit builder: CanMap[A,B,ValA]): builder.Nonterminal = {
      import builder._
      nonterminal (p.name, new AbstractParser[B] {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup) map f
        def symbol = p.symbol; override def reset = p.reset
      })
    }
    
    def map[A,B,ValA](p: AbstractSymbol[A,ValA], f: (Input,A) => B)(implicit builder: CanMap[A,B,ValA]): builder.Nonterminal = {
      import builder._
      nonterminal (p.name, new AbstractParser[B] {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup) map { f(input,_) }
        def symbol = p.symbol; override def reset = p.reset
      })
    }
    
    def flatMap[A,B,ValA,ValB](p: AbstractSequenceBuilder[A,ValA], f: A => AbstractSymbol[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[builder.T] with Slot {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = q(input,i,sppfLookup) flatMap { x => f(x)(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
          def size = q.size + 1; def symbol = org.meerkat.tree.Sequence(q.symbol, org.meerkat.tree.SimpleNonterminal(s"${f.hashCode}"))
          def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, size)
          override def toString = s"[${ruleType.toString()},$size]"
          override def reset = q.reset
        }) }
    }
    
//    def flatMap2[A,B,ValA,ValB](p: AbstractSymbol[A,ValA], f: A => AbstractSymbol[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.SequenceBuilder = {
//      import builder._ 
//      builderSeq { slot =>
//        builder.sequence(new AbstractParser[builder.T] with Slot {
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup) flatMap { x => f(x)(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
//          def size = 2; def symbol = org.meerkat.tree.Sequence(p.symbol, org.meerkat.tree.SimpleNonterminal(s"${f.hashCode}"))
//          def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, size)
//          override def toString = s"[${ruleType.toString()},$size]"
//          override def reset = p.reset
//        }) }
//    }
  }
}
 
object AbstractCPSParsers extends AbstractParsers {  import AbstractParser._
    
  type Result[+T] = CPSResult[T]
  
  def nonterminalSym[A,ValA](name: String, p: => AbstractSymbol[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = nonterminal (name, memoize(alt(q,p))); q
  }
  
  def nonterminalSeq[A,ValA](name: String, p: => AbstractSequenceBuilder[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = nonterminal (name, memoize(alt(q,p))); q
  }
  
  def nonterminalAlt[A,ValA](name: String, p: => AbstractAlternationBuilder[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = builder nonterminal (name, memoize(p(q))); q
  }
  
  def layoutSym[A,ValA <: NoValue](name: String, p: => AbstractSymbol[A,ValA])(implicit builder: CanBuildLayout[A,ValA], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = builder layout (name, memoize(alt(q,p))); q
  }
  
  def layoutSeq[A,ValA <: NoValue](name: String, p: => AbstractSequenceBuilder[A,ValA])(implicit builder: CanBuildLayout[A,ValA], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = builder layout (name, memoize(alt(q,p))); q
  }
  
  def layoutAlt[A,ValA <: NoValue](name: String, p: => AbstractAlternationBuilder[A,ValA])(implicit builder: CanBuildLayout[A,ValA], obj: ClassTag[Result[A]]): builder.Nonterminal = { 
    import builder._
    lazy val q: Nonterminal = builder layout (name, memoize(p(q))); q
  }
  
  def regular[A,ValA](symbol: org.meerkat.tree.NonterminalSymbol, p: => AbstractAlternationBuilder[A,ValA])(implicit builder: CanBuildEBNF[A,ValA], obj: ClassTag[Result[A]]): builder.Regular = { 
    import builder._
    lazy val q: Regular = builder.regular(symbol, memoize(p(q))); q
  }
  
  def groupSeq[A,ValA](p: => AbstractSequenceBuilder[A,_])(implicit builder: CanBuildEBNF[A,ValA], b: CanBuildAlternative[A], obj: ClassTag[Result[A]]): builder.Group = { 
    import builder._
    lazy val q: Group = builder.group(memoize(alt(q, p))); q
  }
  
  def groupAlt[A,ValA](p: => AbstractAlternationBuilder[A,_])(implicit builder: CanBuildEBNF[A,ValA], obj: ClassTag[Result[A]]): builder.Group = { 
    import builder._
    lazy val q: Group = builder.group(memoize(p(q))); q
  }
  
  def preFilter[B,Val](p: AbstractSymbol[B,Val], pred: (Input,Int) => Boolean, prefix: String)(implicit builder: CanBuildNonterminal[B,Val]): builder.Symbol = {
    builder symbol new AbstractParser[B] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = if (pred(input,i)) p(input,i,sppfLookup) else CPSResult.failure[B]
      def name = prefix + " " + p.name; override def toString = name
      def symbol = p.symbol match {
                     case nt: NonterminalSymbol => NonterminalSymbol(name)
                     case TerminalSymbol(_)     => TerminalSymbol(name)
                     case _                     => throw new RuntimeException("Shouldn't have happened!")
                   } 
      def action: Option[Any => Val] = None
      override def reset = p.reset
    }
  }
  
  def postFilter[B,Val](p: AbstractSymbol[B,Val], pred: (Input,B) => Boolean, postfix: String)(implicit builder: CanBuildNonterminal[B,Val]): builder.Symbol = {
    builder symbol new AbstractParser[B] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup) filter { pred(input,_) }
      def name = p.name + " " + postfix; override def toString = name 
      def symbol = p.symbol match {
                     case nt: NonterminalSymbol => NonterminalSymbol(name)
                     case TerminalSymbol(_)     => TerminalSymbol(name)
                     case _                     => throw new RuntimeException("Shouldn't have happened!")
                   } 
      def action: Option[Any => Val] = None
      override def reset = p.reset
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
      override def reset = { 
        val done = results == null
        if (!done) { results = null; q.reset }
      }
    }
  }
  
}