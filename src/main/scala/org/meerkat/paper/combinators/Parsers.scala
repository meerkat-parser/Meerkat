/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.combinators

import scala.language.higherKinds
import sppf.SPPFLookup
import paper.results.MonadPlus
import sppf.NonPackedNode
import paper.results.Fix

trait Parsers {
  
  val input: String
  val sppf: SPPFLookup
    
  type Result[T] <: MonadPlus[T, Result]
  def success[T](t: T): Result[T]
  def failure[T]: Result[T]
  
  type Parser = Int => Result[NonPackedNode]
    
  val fix = Fix.fix[Int, Result[NonPackedNode]] _
    
  private def seq2(p1: Parser, p2: Parser)
    = fix(q => i => p1(i).flatMap(t1 => p2(t1.rightExtent).map(t2 => sppf.getIntermediateNode(q, t1, t2))))
    
  private def rule1(nt: String, p: Parser): Parser
    = fix(q => i => p(i).map(t => sppf.getNonterminalNode(nt, q, t)))
 
  def rule(nt: String, alts: Parser*): Parser 
    = alts.map(rule1(nt, _)).reduce((cur, p) => i => cur(i).orElse(p(i)))
        
  def seq(ps: Parser*): Parser = ps.reduceLeft(seq2)
    
  def terminal(t: String): Parser
    = i => if(input.startsWith(t, i)) success(sppf.getTerminalNode(t, i, i + t.length)) else failure
    
  def epsilon: Parser = i => success(sppf.getEpsilonNode(i))

}