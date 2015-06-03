/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
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