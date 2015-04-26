/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.results

import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.MutableList

trait CPSResults {
  
  type K[T] = T => Unit
  
  def result[T](f: K[T] => Unit): Result[T] = new Result[T] { def apply(k: K[T]) = f(k) }
  
  trait Result[T] extends (K[T] => Unit) with MonadPlus[T, Result] {
    def map[U](f: T => U): Result[U] = result(k => this(memo_k(t => k(f(t)))))
    def flatMap[U](f: T => Result[U]): Result[U] = result(k => this(memo_k(t => f(t)(k))))
    def orElse(r: => Result[T]): Result[T]
           = { lazy val v = r; result(k => { this(k); v(k) }) }
  }
  
  def success[T](t: T): Result[T] = result(k => k(t))
  def failure[T]: Result[T] = result(k => { /* do nothing */ })
  
  def memo_k[T](f: T => Unit): T => Unit = {
    val s: Set[T] = HashSet.empty
    return t => if(!s.contains(t)) { s += t; f(t) }
  }
  
}

trait MemoizedCPSResults extends CPSResults {
  def memo[T](res: => Result[T]): Result[T] = {
    val Rs: MutableList[T] = MutableList.empty
    val Ks: MutableList[K[T]] = MutableList.empty
    return result(k =>
             if(Ks.isEmpty) {
               Ks += k
               val k_i: K[T] = t => if(!Rs.contains(t)) { 
                                         Rs += t; for(kt <- Ks) kt(t) 
                                    }
               res(k_i)
             } else {
               Ks += k
               for(t <- Rs) k(t)
           })
  } 
}
