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

import org.meerkat.sppf.NonPackedNode
import java.util.HashMap
import org.meerkat.util.visualization._
import scala.collection.mutable._
import scala.collection.JavaConversions._

object OperatorParsers {
  
  import AbstractOperatorParsers._
  import Parsers._
  
  object OperatorImplicits {
  
    implicit def obj1[ValA,ValB](implicit vals: ValA|~|ValB) = new CanBuildSequence[NonPackedNode,NonPackedNode,ValA,ValB] {
      implicit val o = Parsers.obj1[ValA,ValB](vals)

      type OperatorSequence = OperatorParsers.OperatorSequence[o.V]
      
      def sequence(p: AbstractOperatorSequence): OperatorSequence 
        = new OperatorSequence { 
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix
            def assoc = p.assoc
          }
      
      def assoc(p: OperatorSequence, a: Assoc.Assoc): OperatorSequence 
        = if ((p.infix && a != Assoc.NON_ASSOC) 
                || ((p.infix || p.prefix || p.postfix) && a == Assoc.NON_ASSOC)) 
            new OperatorSequence {
              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
              def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix
              def assoc: Assoc.Assoc = a
            } 
        else p
      
      type OperatorSequenceBuilder = OperatorParsers.OperatorSequenceBuilder[o.V]
      def builderSeq(f: Head => OperatorSequence): OperatorSequenceBuilder
        = new OperatorSequenceBuilder { def apply(head: Head) = f(head) }
    }
    
    implicit def obj2[ValA,ValB] = new CanBuildAlternation[NonPackedNode,NonPackedNode,ValA,ValB] {
      implicit val o = Parsers.obj3[ValA,ValB]
      
      type OperatorAlternation = OperatorParsers.OperatorAlternation[ValB]     
      def alternation(f: Prec => o.AlternationBuilder): OperatorAlternation 
        = new OperatorAlternation { def apply(prec: Prec) = f(prec) }
      
      type OperatorAlternationBuilder = OperatorParsers.OperatorAlternationBuilder[ValB]
      def builderAlt(f: (Head, Group) => (Group => OperatorAlternation, Group, Option[Group])): OperatorAlternationBuilder
        = new OperatorAlternationBuilder { def apply(head: Head, group: Group) = f(head, group) }
    }
    
    implicit def obj3[Val] = new CanBuildNonterminal[NonPackedNode,Val] {
      implicit val o1 = Parsers.obj5[Val]
      implicit val o2 = Parsers.obj2
      
      type OperatorNonterminal = OperatorParsers.AbstractOperatorNonterminal[Val]   
      def nonterminal(ntName: String, f: Prec => o1.Nonterminal): OperatorNonterminal
        = new OperatorNonterminal {
            val table: java.util.Map[Prec, o1.Nonterminal] = new HashMap()
            def apply(prec: Prec) = if (table.containsKey(prec)) table.get(prec) 
                                    else { val nt = f(prec); table.put(prec, nt); nt }
            def name = ntName; override def toString = ntName
          }
    }  
  }
  
  trait OperatorSequence[+V] extends ((Prec, Prec) => Parsers.SequenceBuilder[V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
  }
  
  trait OperatorSequenceWithAction[+V] extends ((Prec, Prec) => Parsers.SequenceBuilderWithAction[V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
  }
  
  trait OperatorAlternation[+V] extends (Prec => Parsers.AlternationBuilder[V])
  
  trait AbstractOperatorNonterminal[+V] extends (Prec => Parsers.AbstractNonterminal[V]) with EBNFs[V] { import OperatorImplicits._; import AbstractOperatorParser._
    type Abstract[+X] = AbstractOperatorNonterminal[X]
    def name: String
  
    def ~ [U](p: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U, layout: Layout) = this ~~ layout.get ~~ p
    def ~ [U](p: Symbol[U])(implicit tuple: V|~|U, layout: Layout) = this ~~ layout.get ~~ p   
    def ~~ [U](p: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U) = seqNt(this, p)
    def ~~ [U](p: Symbol[U])(implicit tuple: V|~|U) = seqNtSym(this, p)
    
    def | [U >: V](p: OperatorAlternationBuilder[U]) = altOpSymOpAlt(this, p)
    def | [U >: V](p: OperatorSequenceBuilder[U]) = altOpSymOpSeq(this, p)
    def | [U >: V](p: AbstractOperatorNonterminal[U]) = altOpSym(this, p)  
    def | [U >: V](p: OperatorSequenceBuilderWithAction[U]) = altOpSymOpSeq(this, p)
    def | [U >: V](p: OperatorNonterminalWithAction[U]) = altOpSym(this, p)
    
    def | [U >: V](p: AlternationBuilder[U]) = altOpSymOpAlt(this, altAltOpAlt(p))
    def | [U >: V](p: SequenceBuilder[U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: Symbol[U]) = altOpSym(this, altSymOpSym(p))
    def | [U >: V](p: SequenceBuilderWithAction[U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: SymbolWithAction[U]) = altOpSym(this, altSymOpSym(p))
    
    def & [U](f: V => U) = new OperatorNonterminalWithAction[U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) & f
      def name = AbstractOperatorNonterminal.this.name
    }
    
    def ^ [U](f: String => U)(implicit sub: V <:< NoValue) = new OperatorNonterminalWithAction[U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) ^ f
      def name = AbstractOperatorNonterminal.this.name
    }
  }
  
  trait OperatorNonterminalWithAction[+V] extends (Prec => Parsers.SymbolWithAction[V]) { import OperatorImplicits._; import AbstractOperatorParser._
    def name: String
    
    def | [U >: V](p: OperatorAlternationBuilder[U]) = altOpSymOpAlt(this, p)
    def | [U >: V](p: OperatorSequenceBuilder[U]) = altOpSymOpSeq(this, p)
    def | [U >: V](p: AbstractOperatorNonterminal[U]) = altOpSym(this, p)  
    def | [U >: V](p: OperatorSequenceBuilderWithAction[U]) = altOpSymOpSeq(this, p)
    def | [U >: V](p: OperatorNonterminalWithAction[U]) = altOpSym(this, p)
    
    def | [U >: V](p: AlternationBuilder[U]) = altOpSymOpAlt(this, altAltOpAlt(p))
    def | [U >: V](p: SequenceBuilder[U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: Symbol[U]) = altOpSym(this, altSymOpSym(p))
    def | [U >: V](p: SequenceBuilderWithAction[U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: SymbolWithAction[U]) = altOpSym(this, altSymOpSym(p))
  }
  
  type OperatorNonterminal = AbstractOperatorNonterminal[NoValue]
  
  trait OperatorSequenceBuilder[+V] extends (Head => OperatorSequence[V]) { import OperatorImplicits._; import AbstractOperatorParser._
  
    def ~ [U](p: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U, layout: Layout) = this ~~ layout.get ~~ p
    def ~ [U](p: Symbol[U])(implicit tuple: V|~|U, layout: Layout) = this ~~ layout.get ~~ p
    
    def ~~ [U](p: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U) = seqOpSeqNt(this, p)
    def ~~ [U](p: Symbol[U])(implicit tuple: V|~|U) = seqOpSeqSym(this, p)
    
    def | [U >: V](p: OperatorAlternationBuilder[U]) = altOpSeqOpAlt(this, p)
    def | [U >: V](p: OperatorSequenceBuilder[U]) = altOpSeq(this, p)
    def | [U >: V](p: AbstractOperatorNonterminal[U]) = altOpSeqOpSym(this, p)
    def | [U >: V](p: OperatorSequenceBuilderWithAction[U]) = altOpSeq(this, p)
    def | [U >: V](p: OperatorNonterminalWithAction[U]) = altOpSeqOpSym(this, p)
    
    def | [U >: V](p: AlternationBuilder[U]) = altOpSeqOpAlt(this, altAltOpAlt(p))
    def | [U >: V](p: SequenceBuilder[U]) = altOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: Symbol[U]) = altOpSeqOpSym(this, altSymOpSym(p))
    def | [U >: V](p: SequenceBuilderWithAction[U]) = altOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: SymbolWithAction[U]) = altOpSeqOpSym(this, altSymOpSym(p))
    
    def |> [U >: V](p: OperatorAlternationBuilder[U]) = greaterOpSeqOpAlt(this, p)
    def |> [U >: V](p: OperatorSequenceBuilder[U]) = greaterOpSeq(this, p)    
    def |> [U >: V](p: OperatorSequenceBuilderWithAction[U]) = greaterOpSeq(this, p)
    
    def & [U](f: V => U) = new OperatorSequenceBuilderWithAction[U] {
      def apply(head: Head) = {
        val p = OperatorSequenceBuilder.this(head)
        new OperatorSequenceWithAction[U] {
          def apply(prec1: Prec, prec2: Prec) = p(prec1,prec2) & f
          def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix; def assoc = p.assoc
        }
      }
    }
    
    def ^ [U](f: String => U)(implicit sub: V <:< NoValue) = new OperatorSequenceBuilderWithAction[U] {
      def apply(head: Head) = {
        val p = OperatorSequenceBuilder.this(head)
        new OperatorSequenceWithAction[U] {
          def apply(prec1: Prec, prec2: Prec) = p(prec1,prec2) ^ f
          def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix; def assoc = p.assoc
        }
      }
    }
  }
  
  trait OperatorSequenceBuilderWithAction[+V] extends (Head => OperatorSequenceWithAction[V]) { import OperatorImplicits._; import AbstractOperatorParser._
    def | [U >: V](p: OperatorAlternationBuilder[U]) = altOpSeqOpAlt(this, p)
    def | [U >: V](p: OperatorSequenceBuilder[U]) = altOpSeq(this, p)
    def | [U >: V](p: AbstractOperatorNonterminal[U]) = altOpSeqOpSym(this, p)
    def | [U >: V](p: OperatorSequenceBuilderWithAction[U]) = altOpSeq(this, p)
    def | [U >: V](p: OperatorNonterminalWithAction[U]) = altOpSeqOpSym(this, p)
    
    def | [U >: V](p: AlternationBuilder[U]) = altOpSeqOpAlt(this, altAltOpAlt(p))
    def | [U >: V](p: SequenceBuilder[U]) = altOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: Symbol[U]) = altOpSeqOpSym(this, altSymOpSym(p))
    def | [U >: V](p: SequenceBuilderWithAction[U]) = altOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: SymbolWithAction[U]) = altOpSeqOpSym(this, altSymOpSym(p))
    
    def |> [U >: V](p: OperatorAlternationBuilder[U]) = greaterOpSeqOpAlt(this, p)
    def |> [U >: V](p: OperatorSequenceBuilder[U]) = greaterOpSeq(this, p)
    def |> [U >: V](p: OperatorSequenceBuilderWithAction[U]) = greaterOpSeq(this, p)
  }
  
  trait OperatorAlternationBuilder[+V] extends ((Head, Group) => (Group => OperatorAlternation[V], Group, Option[Group])) { import OperatorImplicits._; import AbstractOperatorParser._
    def | [U >: V](p: OperatorAlternationBuilder[U]) = altOpAlt(this, p)
    def | [U >: V](p: OperatorSequenceBuilder[U]) = altOpAltOpSeq(this, p)
    def | [U >: V](p: AbstractOperatorNonterminal[U]) = altOpAltOpSym(this, p)
    def | [U >: V](p: OperatorSequenceBuilderWithAction[U]) = altOpAltOpSeq(this, p)
    def | [U >: V](p: OperatorNonterminalWithAction[U]) = altOpAltOpSym(this, p)
    
    def | [U >: V](p: AlternationBuilder[U]) = altOpAlt(this, altAltOpAlt(p))
    def | [U >: V](p: SequenceBuilder[U]) = altOpAltOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: Symbol[U]) = altOpAltOpSym(this, altSymOpSym(p))
    def | [U >: V](p: SequenceBuilderWithAction[U]) = altOpAltOpSeq(this, altSeqOpSeq(p))
    def | [U >: V](p: SymbolWithAction[U]) = altOpAltOpSym(this, altSymOpSym(p))
    
    def |> [U >: V](p: OperatorAlternationBuilder[U]) = greaterOpAlt(this, p)
    def |> [U >: V](p: OperatorSequenceBuilder[U]) = greaterOpAltOpSeq(this, p)
    def |> [U >: V](p: OperatorSequenceBuilderWithAction[U]) = greaterOpAltOpSeq(this, p)
  }
  
  implicit class ParsersSeqOps[V](p: Parsers.Symbol[V]) { import OperatorImplicits._; import AbstractOperatorParser._
    def ~ [U](q: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U, layout: Layout) = p ~~ layout.get ~~ q
    def ~~ [U](q: AbstractOperatorNonterminal[U])(implicit tuple: V|~|U) = seqSymNt(p, q)
    
    def | [U >: V](q: OperatorAlternationBuilder[U]) = altOpSymOpAlt(altSymOpSym(p), q)
    def | [U >: V](q: OperatorSequenceBuilder[U]) = altOpSymOpSeq(altSymOpSym(p), q)
    def | [U >: V](q: AbstractOperatorNonterminal[U]) = altOpSym(altSymOpSym(p), q)
    def | [U >: V](q: OperatorSequenceBuilderWithAction[U]) = altOpSymOpSeq(altSymOpSym(p), q)
    def | [U >: V](q: OperatorNonterminalWithAction[U]) = altOpSym(altSymOpSym(p), q)
  }
  
  implicit class ParsersAltOps2[V](p: Parsers.AlternationBuilder[V]) { import OperatorImplicits._; import AbstractOperatorParser._
    def | [U >: V](q: OperatorAlternationBuilder[U]) = altOpAlt(altAltOpAlt(p), q)
    def | [U >: V](q: OperatorSequenceBuilder[U]) = altOpAltOpSeq(altAltOpAlt(p), q)
    def | [U >: V](q: AbstractOperatorNonterminal[U]) = altOpAltOpSym(altAltOpAlt(p), q)
    def | [U >: V](q: OperatorSequenceBuilderWithAction[U]) = altOpAltOpSeq(altAltOpAlt(p), q)
    def | [U >: V](q: OperatorNonterminalWithAction[U]) = altOpAltOpSym(altAltOpAlt(p), q)
  }
  
  implicit class ParsersAltOps1[+V](p: Parsers.SequenceBuilder[V]) { import OperatorImplicits._; import AbstractOperatorParser._
    def | [U >: V](q: OperatorAlternationBuilder[U]) = altOpSeqOpAlt(altSeqOpSeq(p), q)
    def | [U >: V](q: OperatorSequenceBuilder[U]) = altOpSeq(altSeqOpSeq(p), q)
    def | [U >: V](q: AbstractOperatorNonterminal[U]) = altOpSeqOpSym(altSeqOpSeq(p), q)
    def | [U >: V](q: OperatorSequenceBuilderWithAction[U]) = altOpSeq(altSeqOpSeq(p), q)
    def | [U >: V](q: OperatorNonterminalWithAction[U]) = altOpSeqOpSym(altSeqOpSeq(p), q)
  }
  
  implicit class StringSeqOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
    val p = Parsers.toTerminal(term)
    def ~ [U](q: AbstractOperatorNonterminal[U])(implicit tuple: NoValue|~|U, layout: Layout) = p ~~ layout.get ~~ q 
    def ~~ [U](q: AbstractOperatorNonterminal[U])(implicit tuple: NoValue|~|U) = seqSymNt(p, q)
  }
  
//  implicit class StringAltOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
//    val p: Symbol { type Value = NoValue } = term
//    
//    def | (q: OperatorAlternationBuilder[NoValue]) = altOpSymOpAlt(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilder[NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: AbstractOperatorNonterminal[NoValue]) = altOpSym(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilderWithAction[NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: OperatorNonterminalWithAction[NoValue]) = altOpSym(altSymOpSym(p), q)
//    
//    def ^ [U](f: String => U)(implicit sub: p.Value <:< NoValue) = p ^ f
//  }
  
  def left[V](p: OperatorSequenceBuilder[V]): OperatorSequenceBuilder[V] = { import OperatorImplicits._
    val o = obj1[V,V](new |~|[V,V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.LEFT))
  }
  
  def right[V](p: OperatorSequenceBuilder[V]): OperatorSequenceBuilder[V] = { import OperatorImplicits._
    val o = obj1[V,V](new |~|[V,V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.RIGHT))
  }
  
  def non_assoc[V](p: OperatorSequenceBuilder[V]): OperatorSequenceBuilder[V] = { import OperatorImplicits._
    val o = obj1[V,V](new |~|[V,V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.NON_ASSOC))
  }
  
  def left[Val](p: OperatorAlternationBuilder[Val]): OperatorAlternationBuilder[Val] = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[Val,Val])(p, Assoc.LEFT)
  }
  
  def right[Val](p: OperatorAlternationBuilder[Val]): OperatorAlternationBuilder[Val] = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[Val,Val])(p, Assoc.RIGHT)
  }
  
  def non_assoc[Val](p: OperatorAlternationBuilder[Val]): OperatorAlternationBuilder[Val] = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[Val,Val])(p, Assoc.NON_ASSOC)
  }
  
  def ntAlt[Val](name: String, p: => OperatorAlternationBuilder[Val]): AbstractOperatorNonterminal[Val] = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalAlt
    nonterminalAlt(name, p)
  }
  def ntSeq[Val](name: String, p: => OperatorSequenceBuilder[Val]): AbstractOperatorNonterminal[Val] = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSym[Val](name: String, p: => AbstractOperatorNonterminal[Val]): AbstractOperatorNonterminal[Val] = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }
  def ntSeqWithAction[Val](name: String, p: => OperatorSequenceBuilderWithAction[Val]): AbstractOperatorNonterminal[Val] = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSymWithAction[Val](name: String, p: => OperatorNonterminalWithAction[Val]): AbstractOperatorNonterminal[Val] = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }
  
  trait EBNFs[+V] { self: AbstractOperatorNonterminal[V] =>
    var star: Option[AbstractOperatorNonterminal[_]] = None
    def *(implicit ebnf: EBNF[V], layout: Layout): AbstractOperatorNonterminal[ebnf.OptOrSeq] = {
      star.getOrElse({
        val p = new AbstractOperatorNonterminal[ebnf.OptOrSeq] { def apply(prec: Prec) = self($).*(ebnf,layout); def name = self.name + "*" }
        star = Option(p); p
        }).asInstanceOf[AbstractOperatorNonterminal[ebnf.OptOrSeq]]
    }
    
    var star_sep: Map[String,AbstractOperatorNonterminal[_]] = new java.util.HashMap[String,AbstractOperatorNonterminal[_]]()
    def *(sep: String)(implicit ebnf: EBNF[V], layout: Layout): AbstractOperatorNonterminal[ebnf.OptOrSeq] = {
      star_sep.getOrElseUpdate(sep, new AbstractOperatorNonterminal[ebnf.OptOrSeq] { 
        def apply(prec: Prec) = self($).*(sep)(ebnf,layout); def name = s"{${self.name} $sep}*" }).asInstanceOf[AbstractOperatorNonterminal[ebnf.OptOrSeq]]
    }
    
    var plus: Option[AbstractOperatorNonterminal[_]] = None
    def +(implicit ebnf: EBNF[V], layout: Layout): AbstractOperatorNonterminal[ebnf.OptOrSeq] = {
      plus.getOrElse({
        val p = new AbstractOperatorNonterminal[ebnf.OptOrSeq] { def apply(prec: Prec) = self($).+; def name = self.name + "+" }
        plus = Option(p); p
        }).asInstanceOf[AbstractOperatorNonterminal[ebnf.OptOrSeq]]
    }
    
    var plus_sep: Map[String,AbstractOperatorNonterminal[_]] = new java.util.HashMap[String,AbstractOperatorNonterminal[_]]()
    def +(sep: String)(implicit ebnf: EBNF[V], layout: Layout): AbstractOperatorNonterminal[ebnf.OptOrSeq] = {
      plus_sep.getOrElseUpdate(sep, new AbstractOperatorNonterminal[ebnf.OptOrSeq] { 
        def apply(prec: Prec) = self($).+(sep)(ebnf,layout); def name = s"{${self.name} $sep}+" }).asInstanceOf[AbstractOperatorNonterminal[ebnf.OptOrSeq]]
    }
  }
}