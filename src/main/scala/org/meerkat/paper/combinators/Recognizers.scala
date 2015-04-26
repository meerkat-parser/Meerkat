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

trait Recognizers {
  
  type Result[T] <: MonadPlus[T, Result]
  def success[T](t: T): Result[T]
  def failure[T]: Result[T]

  val input: String
  
  type Recognizer = Int => Result[Int]
  
  private def seq(r1: Recognizer, r2: Recognizer): Recognizer
    = i => r1(i).flatMap(r2)
    
  def rule(nt: String, alts: Recognizer*): Recognizer
    = alts.reduce((cur, alt) => i => cur(i).orElse(alt(i)))
    
  def seq(rs: Recognizer*): Recognizer
    = rs.reduceLeft(seq)
    
  def terminal(t: String): Recognizer
    = i => if(input.startsWith(t, i)) success(i + t.length) else failure
    
  def epsilon: Recognizer = i => success(i)
  
}
