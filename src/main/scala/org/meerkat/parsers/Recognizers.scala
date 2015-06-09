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

//package org.meerkat.tmp
//
//import org.meerkat.util.Input
//import org.meerkat.sppf.SPPFLookup
//import org.meerkat.sppf.Slot
//import org.meerkat.tree.RuleType
//
//object Recognizers {
//  
//  import AbstractCPSParsers._
//  
//  implicit object obj1 extends CanBuildSequence[Int, Int] {
//    
//    type T = Int
//    type Sequence = Recognizers.Sequence
//    
//    def sequence(p: AbstractSequence[Int]): Sequence 
//      = new Sequence { 
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol
//          def ruleType = p.ruleType
//          def size = p.size
//        }
//    
//    def index(a: T): Int = a
//    def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): Int = b
//    
//    type SequenceBuilder = Recognizers.SequenceBuilder
//    def builderSeq(f: Slot => Sequence): SequenceBuilder = new SequenceBuilder { def apply(slot: Slot) = f(slot) } 
//  }
//  
//  implicit object obj2 extends CanBuildAlternation[Int] {
//    type Alternation = Recognizers.Alternation
//    
//    def alternation(p: AbstractParser[Int]): Alternation
//      = new Alternation {
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
//        }
//    
//    def result(e: Int, p: Slot, nt: Head, sppfLookup: SPPFLookup): Int = e
//    
//    type AlternationBuilder = Recognizers.AlternationBuilder
//    def builderAlt(f: Head => Alternation): AlternationBuilder = new AlternationBuilder { def apply(head: Head) = f(head) }
//  }
//  
//  implicit object obj3 extends Memoizable[Int] {
//    type U = Int
//    def value(t: Int) = t
//  }
//  
//  implicit object obj4 extends CanBuildNonterminal[Int] {
//    type Nonterminal = Recognizers.Nonterminal
//    def nonterminal(nt: String, p: AbstractParser[Int]): Nonterminal 
//      = new Nonterminal {
//          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
//          def symbol = org.meerkat.tree.Nonterminal(nt)
//          def name = nt
//        }
//  }
//  
//  trait Sequence extends AbstractParser[Int] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
//  
//  trait Alternation extends AbstractParser[Int] { def symbol: org.meerkat.tree.Alt }
//  
//  trait Nonterminal extends Symbol { def symbol: org.meerkat.tree.Nonterminal }
//  
//  trait Terminal extends Symbol { def symbol: org.meerkat.tree.Terminal }
//  
//  trait SequenceBuilder extends (Slot => Sequence) { import AbstractParser._
//    def ~ (p: Symbol): SequenceBuilder = seq(this, p)
//    
//    def | (p: AlternationBuilder): AlternationBuilder = altSeqAlt(this, p)
//    def | (p: SequenceBuilder): AlternationBuilder = altSeq(this, p)
//    def | (p: Symbol): AlternationBuilder = altSeqSym(this, p)
//  }
//  
//  trait AlternationBuilder extends (Head => Alternation) { import AbstractParser._
//    def | (p: AlternationBuilder): AlternationBuilder = altAlt(this, p)
//    def | (p: SequenceBuilder): AlternationBuilder = altAltSeq(this, p)
//    def | (p: Symbol): AlternationBuilder = altAltSym(this, p)
//  }
//  
//  trait Symbol extends AbstractParser[Int] { import AbstractParser._
//    def name: String 
//    
//    def ~ (p: Symbol): SequenceBuilder = seq(this, p)
//    
//    def | (p: AlternationBuilder): AlternationBuilder = altSymAlt(this, p)
//    def | (p: SequenceBuilder): AlternationBuilder = altSymSeq(this, p)
//    def | (p: Symbol): AlternationBuilder = altSym(this, p)
//  }
//  
//  def ntAlt(name: String, p: => AlternationBuilder): Nonterminal = nonterminalAlt(name, p)
//  def ntSeq(name: String, p: => SequenceBuilder): Nonterminal = nonterminalSeq(name, p)
//  def ntSym(name: String, p: Symbol): Nonterminal = nonterminalSym(name, p)
//
//}