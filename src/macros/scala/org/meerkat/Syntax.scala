/** 
 * @author Anastasia Izmaylova
 */

package org.meerkat

import org.meerkat.sppf.NonPackedNode
import org.meerkat.tmp.&
import org.meerkat.tmp.Parsers.AlternationBuilder
import org.meerkat.tmp.Parsers.SequenceBuilder
import org.meerkat.tmp.OperatorParsers.OperatorAlternationBuilder
import org.meerkat.tmp.OperatorParsers.OperatorSequenceBuilderWithAction
import org.meerkat.tmp.OperatorParsers.AbstractOperatorNonterminal
import org.meerkat.tmp.OperatorParsers.OperatorNonterminalWithAction
import org.meerkat.tmp.Parsers.Nonterminal
import org.meerkat.tmp.OperatorParsers.OperatorSequenceBuilder
import org.meerkat.tmp.OperatorParsers.OperatorNonterminal
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
import org.meerkat.tmp.AbstractCPSParsers.AbstractSymbol
import scala.reflect.macros.blackbox.Context

object Syntax {
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
    
    def syn[T](p: AlternationBuilder { type Value = T }) = macro makeNonterminalAltWithName[T]
    def syn[T](p: SequenceBuilder { type Value = T }) = macro makeNonterminalSeqWithName[T]
    def syn[T](p: AbstractSymbol[NonPackedNode] { type Value = T }) = macro makeNonterminalSymWithName[T]
    
    def makeNonterminalAltWithName[T](c: Context)(p: c.Expr[AlternationBuilder]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntAlt")
    def makeNonterminalSeqWithName[T](c: Context)(p: c.Expr[SequenceBuilder]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSeq")
    def makeNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSym")
    
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