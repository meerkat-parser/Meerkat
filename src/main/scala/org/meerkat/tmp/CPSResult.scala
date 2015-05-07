package org.meerkat.tmp

import java.util.Deque
import java.util.ArrayDeque
import java.util.Set 
import java.util.LinkedHashSet

trait Memoizable[-T] { 
  type U 
  def value(t: T): U 
}

trait CPSResult[+T] extends ((T => Unit) => Unit) with MonadPlus[T, CPSResult] { import CPSResult._
  
  def map[U](f: T => U)(implicit m: Memoizable[T]) = result[U](k => this(memo_k(k compose f)))
  
  def flatMap[U](f: T => CPSResult[U])(implicit m: Memoizable[T]) = result[U](k => this(memo_k(t => f(t)(k))))
  
  def orElse[U >: T](r: => CPSResult[U]) = result[U](k => Trampoline.alt(this, k, r))
  
  def filter(pred: T => Boolean) = result[T](k => this(t => if(pred(t)) k(t)))
  
  // Optimization
  def _map[U](f: T => U) = result[U](k => this(k compose f))
}

object CPSResult {
  
  type K[T] = T => Unit
  
  def result[T](f: K[T] => Unit): CPSResult[T] = new CPSResult[T] { def apply(k: K[T]) = f(k) }
  def success[T](t: T): CPSResult[T] = new CPSResult[T] { def apply(k: K[T]) = k(t) }
  def failure[T]: CPSResult[T] = new CPSResult[T] { def apply(k: K[T]) = () }
  
  def memo[T](res: => CPSResult[T]): CPSResult[T] = {
    val Ks: Deque[K[T]] = new ArrayDeque[K[T]]()
    val Rs: Set[T] = new LinkedHashSet[T]()
    
    new CPSResult[T] { 
      def apply(k: K[T]) = { 
        if(Ks.isEmpty) {
          Ks.push(k)
          res(t => 
                if(!Rs.contains(t)) {
                  Rs.add(t)
                  val it = Ks.iterator()
                  while(it hasNext) Trampoline.call(it.next(), t) 
                })
        } else { 
          Ks.push(k) 
          val it = Rs.iterator()
          while(it hasNext) Trampoline.call(k, it.next())
        } 
      } 
    }
  }
  
  protected def memo_k[T](f: T => Unit)(implicit m: Memoizable[T]): T => Unit = {
    val s: java.util.Set[m.U] = new java.util.HashSet[m.U]()
    t => if(!s.contains(m.value(t))) { s.add(m.value(t)); f(t)}
  }
}
