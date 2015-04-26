/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.main

import paper.combinators.CPSRecognizers
import paper.combinators.MemoizedCPSRecognizers
import paper.combinators.MemoizedCPSParsers
import sppf.OriginalSPPFLookup
import sppf.NonPackedNode
import paper.results.Fix

object UseCPSRecognizers extends CPSRecognizers {
  val fix = Fix.fix[Int, Result[Int]] _
  
  override val input = "++x"
  
  val p = fix(exp => rule("Exp", seq(terminal("+"), exp), terminal("x")))
  
  def main(args: Array[String]) {
    p(0)(i => println(i))
  }
  
}

object UseMemoizedCPSRecognizers extends MemoizedCPSRecognizers { 
  val fix = Fix.fix[Int, Result[Int]] _
  
  override val input = "bbbbbbbbbb"
  
  val p = fix(exp => rule("Exp", seq(exp, terminal("+"), exp), terminal("x")))
  
  /**
   * C(n+m+1, m+1) + C(n+3,3) + 2*n + 1
   * n = 5,  m = 3 ==> C(9,4)  + C(8,3)  + 11 = 193    versus 123
   * n = 10, m = 3 ==> C(14,4) + C(13,3) + 21 = 1308   versus 593
   * n = 20, m = 3 ==> C(24,4) + C(23,3) + 41 = 12438  versus 3583
   * 
   * n = 5,  m = 5 ==> C(11,6) + C(8,3)  + 11 = 529    versus 123
   * n = 10, m = 5 ==> C(16,6) + C(13,3) + 21 = 8315   versus 593
   * n = 20, m = 5 ==> C(26,6) + C(23,3) + 41 = 232042 versus 3583
   */
  
  /**
   * continuations: 6 * (n+1)
   * k_i's: n + 1
   */
  
  
  val s = fix(s => rule("S", seq(s, s, s, s, s), seq(s, s), terminal("b"), epsilon))
  
  def main(args: Array[String]) {
    s(0)({ i => })
  }
  
}

object UseMemoizedCPSParsers extends MemoizedCPSParsers {
  
  override val sppf = new OriginalSPPFLookup
  override val input = "x+x"
  
 /**
  * Gamma2 with epsilon (without prefix sharing): 
  *   2*( C(n+k,k) + C(n+k-1,k-1) + n ) + 1 + C(n+k,k) 
  *   for example, 5 b's - 221
  */
  
  val exp = fix(exp => rule("Exp", seq(exp, terminal("+"), exp), terminal("x")))
  val s = fix(s => rule("S", seq(s, s, s), seq(s, s), terminal("b"), epsilon))
  
  var symbol: Option[NonPackedNode] = None
  
  def runS(): Unit = {
    s(0)({ _ => })
    symbol = sppf.getStartNode("S", 0, input.length)  
  }
  
  def runExp(): Unit = {
    exp(0)({ _ => }) 
    symbol = sppf.getStartNode("Exp", 0, input.length)
  }
  
  def main(args: Array[String]) = runS()
  
}