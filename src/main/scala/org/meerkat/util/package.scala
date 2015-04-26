/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
import java.lang.management.ThreadMXBean
import java.lang.management.ManagementFactory

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
  
}