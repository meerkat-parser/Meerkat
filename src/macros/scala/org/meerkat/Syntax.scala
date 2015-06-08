/** 
 * @author Anastasia Izmaylova
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

object Syntax { 
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    def syn[T](p: Parsers.AlternationBuilder[T]) = macro makeNonterminalAltWithName[T]
    def syn[T](p: Parsers.SequenceBuilder[T]) = macro makeNonterminalSeqWithName[T]
    def syn[T](p: AbstractSymbol[NonPackedNode,T]) = macro makeNonterminalSymWithName[T]
    
    def syn[T,V](p: DDParsers.AlternationBuilder[T,V]) = macro makeDataNonterminalAltWithName[T,V]
    def syn[T,V](p: DDParsers.SequenceBuilder[T,V]) = macro makeDataNonterminalSeqWithName[T,V]
    def syn[T,V](p: AbstractSymbol[(NonPackedNode,T),V]) = macro makeDataNonterminalSymWithName[T,V]
    
    def makeNonterminalAltWithName[T](c: Context)(p: c.Expr[AlternationBuilder[T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntAlt")
    def makeNonterminalSeqWithName[T](c: Context)(p: c.Expr[SequenceBuilder[T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSeq")
    def makeNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode,T]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSym")
    
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