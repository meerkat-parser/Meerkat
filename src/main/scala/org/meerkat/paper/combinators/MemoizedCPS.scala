/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.combinators

import paper.results.MemoizedCPSResults
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

trait MemoizedCPS extends MemoizedCPSResults {
  def memo[T](f: Int => Result[T]): Int => Result[T] = {
    val table: Map[Int, Result[T]] = HashMap.empty
    i => table.getOrElseUpdate(i, memo(f(i)))
  }
}