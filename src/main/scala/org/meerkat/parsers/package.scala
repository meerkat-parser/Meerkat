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

package org.meerkat

import org.meerkat.util._
import org.meerkat.util.Input
import org.meerkat.util.visualization._
import org.meerkat.sppf.SPPFLookup
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.sppf.SemanticAction
import org.meerkat.sppf.TreeBuilder
import org.meerkat.sppf.NonPackedNode
import org.meerkat.parsers.ParseError
import org.meerkat.parsers.ParseSuccess
import org.meerkat.parsers.ParseStatistics
import org.meerkat.parsers.AbstractCPSParsers
import org.meerkat.parsers.OperatorParsers
import org.meerkat.parsers.Trampoline
import org.meerkat.parsers.Parsers

package object parsers {
  
  case class ~[+A,+B](_1: A, _2: B)
  
  trait <:<[-A,B]  
  trait <:!<[A,B]

  type ![T] = { type f[U] = U <:!< T }
  
  sealed trait NoValue
  
  trait |~|[-A,B] { type R }
  
  type &[A <: { type Abstract[_] },T] = A#Abstract[T]
  
  type && [T] = { type action[F] }
  
  trait EBNF[-Val] {
    type OptOrSeq; type Seq; type Group
    val add: ((OptOrSeq,Val)) => OptOrSeq
    val unit: Val => OptOrSeq
    val empty: String => OptOrSeq
    val group: Val => Group
  }
  
  object |~| {
    implicit def f1[A <: NoValue,B <: NoValue] = new |~|[NoValue,NoValue] { type R = NoValue }
    implicit def f2[A <: NoValue,B: ![NoValue]#f] = new |~|[NoValue,B] { type R = B }
    implicit def f3[A: ![NoValue]#f,B <: NoValue] = new |~|[A,NoValue] { type R = A }
    implicit def f4[A: ![NoValue]#f,B: ![NoValue]#f] = new |~|[A,B] { type R = (A,B) }
  }
  
  object <:< {
    implicit def sub[A,B >: A]: A <:< B = null
  }
  
  object <:!< {
    implicit def nsub[A,B]: A <:!< B = null
    implicit def nsubAmb1[A,B >: A]: A <:!< B = null
    implicit def nsubAmb2[A,B >: A]: A <:!< B = null
  }
  
  object EBNF {
    implicit val ebnf1 = new EBNF[NoValue] { 
      type OptOrSeq = NoValue; type Group = NoValue
      val add: ((OptOrSeq,NoValue)) => OptOrSeq = _ => null
      val unit: NoValue => OptOrSeq = _ => null
      val empty: String => OptOrSeq = _ => null
      val group: NoValue => Group = _ => null
    }
  
    implicit def ebnf2[Val: ![NoValue]#f] = new EBNF[Val] { 
      type OptOrSeq = List[Val]; type Group = Val
      val add: ((OptOrSeq,Val)) => OptOrSeq = { case (s,x) => s.:+(x) }
      val unit: Val => OptOrSeq = x => List(x)
      val empty: String => OptOrSeq = _ => List()
      val group: Val => Group = x => x
    }
  }
  
  type Prec = (Int, Int)
  val $: Prec = (0,0)
  
  trait Layout { def get: Parsers.Symbol[NoValue] }
  def layout(p: Parsers.Symbol[NoValue]): Layout = new Layout {
    def get = p
  }
  
  def start[T](p: Parsers.Symbol[T])(implicit layout: Layout): Parsers.AbstractNonterminal[T] 
    = Parsers.ntSeq(s"start[${p.name}]", layout.get ~~ p ~~ layout.get)
  
  object Layout {
    implicit val L: Layout = layout(Parsers.ntSym("L",Parsers.toTerminal("""((/\*(.|[\r\n])*?\*/|//[^\r\n]*)|\s)*""".r)))  
  }
  
  def run[T](input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[T]): Unit = {
    parser(input, 0, sppf)(t => {})
    Trampoline.run
  }
  
  def parse[Val](sentence: String, parser: OperatorParsers.AbstractOperatorNonterminal[Val]): Unit 
    = parse(sentence, parser((0,0)))
  
  def parse[T,V](sentence: String, parser: AbstractCPSParsers.AbstractSymbol[T,V]): Unit = {
    val input = new Input(sentence)
    val sppf = new DefaultSPPFLookup(input)
    
    run(input, sppf, parser)
    
    println(s"Trying to find: ${parser.name}(0,${sentence.length()})")
    val startSymbol = sppf.getStartNode(parser, 0, sentence.length())
    
//    println("Resetting ...")
//    parser.reset
    
    startSymbol match {
      case None       => println("Parse error")
      case Some(node) => println("Success: " + node)
                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                         println("Visualizing...")
                         visualize(node, input)
                         visualize(TreeBuilder.build(node)(input), input, memoize = false)
                         val x = SemanticAction.execute(node)(input)
                         println(s"WOW: $x")
                         println("Done!")
    }
  }
  
  def parse[T](parser: Parsers.AbstractNonterminal[T], input: Input): Either[ParseError, ParseSuccess] = {

    parser.reset
    
    val sppf = new DefaultSPPFLookup(input)
    
    val startUserTime   = getUserTime
    val startSystemTime = getCpuTime
    val startNanoTime   = System.nanoTime
    
    run(input, sppf, parser)
    
    val endUserTime     = getUserTime
    val endSystemTime   = getCpuTime
    val endNanoTime     = System.nanoTime
    
    val startSymbol = sppf.getStartNode(parser, 0, input.length)
    
    startSymbol match {
      case None    => Left(ParseError(0, " "))
      case Some(x) => Right(ParseSuccess(x, ParseStatistics((endNanoTime - startNanoTime) / 1000000, 
                                                            (endUserTime - startUserTime) / 1000000,
                                                            (1000000) / 1000000,
                                                            sppf.countNonterminalNodes,
                                                            sppf.countIntermediateNodes,
                                                            sppf.countTerminalNodes,
                                                            sppf.countPackedNodes,
                                                            sppf.countAmbiguousNodes)))
    }
  }
  
}