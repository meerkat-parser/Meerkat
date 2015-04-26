/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import java.util.Deque
import java.util.ArrayDeque

trait Runnable { def run: Unit }
  
class Call[T](k: T => Unit, t: T) extends Runnable { def run: Unit = k(t) }
class Seq[T](r: => ((T => Unit) => Unit), k: T => Unit) extends Runnable { def run: Unit = r(k) }
class Alt[T](lhs: (T => Unit) => Unit, k: T => Unit, rhs: => ((T => Unit) => Unit)) extends Runnable {
  def run: Unit = { Trampoline.jobs.push(new Seq(rhs, k)); lhs(k) }
}
    
object Trampoline {
  val jobs: Deque[Runnable] = new ArrayDeque[Runnable]()
    
  def call[T](k: T => Unit, t: T): Unit = Trampoline.jobs.push(new Call(k, t))
  def alt[T](lhs: (T => Unit) => Unit, k: T => Unit, rhs: => ((T => Unit) => Unit)): Unit
    = Trampoline.jobs.push(new Alt(lhs, k, rhs))
    
  def run: Unit = while(!jobs.isEmpty) jobs.pop.run    
}