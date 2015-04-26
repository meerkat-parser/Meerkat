/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.combinators

import scala.language.higherKinds
import paper.results.MonadPlus
import sppf.SPPFLookup
import sppf.NonPackedNode
import paper.results.CPSResults
import paper.results.Fix

trait CPSParsers extends Parsers with CPSResults

trait MemoizedCPSParsers extends CPSParsers with MemoizedCPS {  
  override def rule(nt: String, alts: Parser*) = memo(super.rule(nt, alts:_*))
}

trait CPSDDParsers extends CPSParsers {
  
  def fixpoint[T] = Fix.fix[Int, Result[(NonPackedNode, T)]] _
  
  type DDParser[T] = Int => Result[(NonPackedNode, T)]
  
  def seq[T,U](p: DDParser[T], f: T => DDParser[U]): DDParser[U]
    = fixpoint[U](q => i => p(i).flatMap(t1 => f(t1._2)(t1._1.rightExtent).map(t2 => (sppf.getIntermediateNode(q, t1._1, t2._1), t2._2))))
    
  def seq_left[T,U](p1: DDParser[T], p2: DDParser[U]): DDParser[T] = seq(p1, (t:T) => map(p2, (u:U) => t))
  
  def seq_right[T,U](p1: DDParser[T], p2: DDParser[U]): DDParser[U] = seq(p1, (t:T) => p2)
    
  def map[T, U](p: DDParser[T], f: T => U): DDParser[U] = i => p(i).map(t => (t._1, f(t._2)))
  
  def toDDParser(p: Parser): DDParser[Unit] = i => p(i).map(t => (t, ()))
  def toParser[T](p: DDParser[T]): Parser = i => p(i).map(t => t._1)
  
}
