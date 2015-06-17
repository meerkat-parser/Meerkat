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

import org.meerkat.sppf.NonPackedNode
import org.meerkat.parsers.&
import org.meerkat.parsers.Parsers
import org.meerkat.parsers.DDParsers
import org.meerkat.parsers.Parsers.AlternationBuilder
import org.meerkat.parsers.Parsers.SequenceBuilder
import org.meerkat.parsers.OperatorParsers.OperatorAlternationBuilder
import org.meerkat.parsers.OperatorParsers.OperatorSequenceBuilderWithAction
import org.meerkat.parsers.OperatorParsers.AbstractOperatorNonterminal
import org.meerkat.parsers.OperatorParsers.OperatorNonterminalWithAction
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.parsers.DDParsers.DataNonterminal
import org.meerkat.parsers.DDParsers.DataNonterminalWithAction
import org.meerkat.parsers.OperatorParsers.OperatorSequenceBuilder
import org.meerkat.parsers.OperatorParsers.OperatorNonterminal
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
import org.meerkat.parsers.AbstractCPSParsers.AbstractSymbol
import scala.reflect.macros.blackbox.Context
import org.meerkat.parsers.NoValue
import org.meerkat.parsers.Layout

object Syntax { 
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    def syn[T](p: Parsers.AlternationBuilder[T]) = macro makeNonterminalAltWithName[T]
    def syn[T](p: Parsers.SequenceBuilder[T]) = macro makeNonterminalSeqWithName[T]
    def syn[T](p: AbstractSymbol[NonPackedNode,T]) = macro makeNonterminalSymWithName[T]
    
    // TODO: rename
    def lay(p: Parsers.AlternationBuilder[NoValue]) = macro makeLayoutAltWithName
    def lay(p: Parsers.SequenceBuilder[NoValue]) = macro makeLayoutSeqWithName
    def lay(p: AbstractSymbol[NonPackedNode,NoValue]) = macro makeLayoutSymWithName
    
    def syn[T,V](p: DDParsers.AlternationBuilder[T,V]) = macro makeDataNonterminalAltWithName[T,V]
    def syn[T,V](p: DDParsers.SequenceBuilder[T,V]) = macro makeDataNonterminalSeqWithName[T,V]
    def syn[T,V](p: AbstractSymbol[(NonPackedNode,T),V]) = macro makeDataNonterminalSymWithName[T,V]
    
    def makeNonterminalAltWithName[T](c: Context)(p: c.Expr[AlternationBuilder[T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntAlt")
    def makeNonterminalSeqWithName[T](c: Context)(p: c.Expr[SequenceBuilder[T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSeq")
    def makeNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode,T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSym")
    
    def makeLayoutAltWithName(c: Context)(p: c.Expr[AlternationBuilder[NoValue]]): c.Expr[Layout] = makeCallWithName (c, "Parsers.ltAlt")
    def makeLayoutSeqWithName(c: Context)(p: c.Expr[SequenceBuilder[NoValue]]): c.Expr[Layout] = makeCallWithName (c, "Parsers.ltSeq")
    def makeLayoutSymWithName(c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode,NoValue]]): c.Expr[Layout] = makeCallWithName (c, "Parsers.ltSym")
    
    def makeDataNonterminalAltWithName[T,V](c: Context)(p: c.Expr[DDParsers.AlternationBuilder[T,V]]): c.Expr[DataNonterminalWithAction[T,V]] = makeCallWithName (c, "DDParsers.ntAlt")
    def makeDataNonterminalSeqWithName[T,V](c: Context)(p: c.Expr[DDParsers.SequenceBuilder[T,V]]): c.Expr[DataNonterminalWithAction[T,V]] = makeCallWithName (c, "DDParsers.ntSeq")
    def makeDataNonterminalSymWithName[T,V](c: Context)(p: c.Expr[AbstractSymbol[(NonPackedNode,T),V]]): c.Expr[DataNonterminalWithAction[T,V]] = makeCallWithName (c, "DDParsers.ntSym")
    
    def syn[T](p: OperatorAlternationBuilder[T]) = macro makeOperatorNonterminalAltWithName[T]
    def syn[T](p: OperatorSequenceBuilder[T]) = macro makeOperatorNonterminalSeqWithName[T]
    def syn[T](p: AbstractOperatorNonterminal[T]) = macro makeOperatorNonterminalSymWithName[T]
    def syn[T](p: OperatorSequenceBuilderWithAction[T]) = macro makeOperatorNonterminalSeqWithActionWithName[T]
    def syn[T](p: OperatorNonterminalWithAction[T]) = macro makeOperatorNonterminalSymWithActionWithName[T]
    
    def makeOperatorNonterminalAltWithName[T](c: Context)(p: c.Expr[OperatorAlternationBuilder[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntAlt")
    def makeOperatorNonterminalSeqWithName[T](c: Context)(p: c.Expr[OperatorSequenceBuilder[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSeq")
    def makeOperatorNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractOperatorNonterminal[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSym")
    def makeOperatorNonterminalSeqWithActionWithName[T](c: Context)(p: c.Expr[OperatorSequenceBuilderWithAction[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSeqWithAction")
    def makeOperatorNonterminalSymWithActionWithName[T](c: Context)(p: c.Expr[OperatorNonterminalWithAction[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSymWithAction")
}