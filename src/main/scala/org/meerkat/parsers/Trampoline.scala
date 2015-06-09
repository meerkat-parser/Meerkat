/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.parsers

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