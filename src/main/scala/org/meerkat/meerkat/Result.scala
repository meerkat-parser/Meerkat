/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import scala.collection.mutable.Buffer
import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import sppf.NonPackedNode
import java.util.Deque
import java.util.ArrayDeque


trait Indexing[T] { def rightExtent(t: T): Int }

object Indexing {
  implicit object IndexingSPPF extends Indexing[NonPackedNode] {
    def rightExtent(t: NonPackedNode): Int = t.rightExtent
  }
  implicit def indexingTuple[T]: Indexing[(NonPackedNode, T)] 
    = new Indexing[(NonPackedNode, T)] { 
        def rightExtent(t: (NonPackedNode, T)) = t._1.rightExtent  
      }
}

object Memo {
  def memo_k[T](f: T => Unit)(implicit ind: Indexing[T]): T => Unit = {
    val s: Set[Int] = new java.util.HashSet[Int]()
    t => if(!s.contains(ind.rightExtent(t))) { 
           s.add(ind.rightExtent(t))
           f(t)
         }
  }  
}

trait Result[T] extends ((T => Unit) => Unit) {
  
  import Memo.memo_k
  import Result.result
  
  def mapNoMemo[U](f: T => U): Result[U] = result(k => this(k.compose(f))) // Specializing for optimization
  def map[U](f: T => U)(implicit ind: Indexing[T]): Result[U] = result(k => this(memo_k(k.compose(f))))
  def flatMap[U](f: T => Result[U])(implicit ind: Indexing[T]): Result[U] = result(k => this(memo_k(t => f(t)(k))))
  def orElse(rhs: => Result[T]): Result[T] = result(k => Trampoline.alt(this, k, rhs))
  def filter(p: T => Boolean): Result[T] = result(k => this(t => if(p(t)) k(t)))
}

object Result {
  
  type K[T] = T => Unit
  
  def result[T](f: K[T] => Unit): Result[T] = new Result[T] { def apply(k: K[T]): Unit = f(k) }
  def success[T](t: T): Result[T] = new Result[T] { def apply(k: K[T]): Unit = k(t) }
  def failure[T]: Result[T] = new Result[T] { def apply(k: K[T]): Unit = { ; } }
  
  def memo[T](res: => Result[T]): Result[T] = {
    val Ks: Deque[K[T]] = new ArrayDeque[K[T]]()
    val Rs: java.util.Set[T] = new java.util.LinkedHashSet[T]()
    new Result[T] { 
      def apply(k: K[T]) = { 
        if(Ks.isEmpty) {
          Ks.push(k)
          res((t: T) => { 
                if(!Rs.contains(t)) {
                  Rs.add(t)
                  val iter = Ks.iterator()
                  while(iter.hasNext()) Trampoline.call(iter.next(), t) 
                }
              })
        } else { 
          Ks.push(k) 
          val iter = Rs.iterator()
          while(iter.hasNext()) Trampoline.call(k, iter.next())
        } 
      } 
    }
  } 

}