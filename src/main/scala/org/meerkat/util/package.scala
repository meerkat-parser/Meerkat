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

package org.meerkat

import java.lang.management.ThreadMXBean
import java.lang.management.ManagementFactory
import java.io.File
import scala.collection.mutable.ListBuffer
import org.apache.commons.io.FileUtils

package object util {

  implicit def doItLazy[T](v: => T) = new Lazy(v)
  
  def getUserTime: Long = {
    val bean: ThreadMXBean = ManagementFactory.getThreadMXBean
	return if (bean.isCurrentThreadCpuTimeSupported)
	         bean.getCurrentThreadUserTime()
	       else 0L
  }
  
  def getCpuTime: Long = {
    val bean: ThreadMXBean = ManagementFactory.getThreadMXBean
	return if (bean.isCurrentThreadCpuTimeSupported)
	         bean.getCurrentThreadCpuTime()
	       else 0L
  }
  
  def getSystemTime: Long = {
    val bean: ThreadMXBean = ManagementFactory.getThreadMXBean
    return if (bean.isCurrentThreadCpuTimeSupported( ))
        (bean.getCurrentThreadCpuTime() - bean.getCurrentThreadUserTime( )) else 0L
  }
  
  
   implicit class Load(dir: String) {
     def load(ext: String, rec: Boolean = true): scala.Seq[File] = {
        val files = FileUtils.listFiles(new File(dir), Array(ext), rec)
        val it = files.iterator
        val inputPaths: ListBuffer[File] = new ListBuffer
        
        while(it.hasNext()) {
          inputPaths += (it.next().asInstanceOf[File])
        }
        inputPaths
     } 
   } 
}