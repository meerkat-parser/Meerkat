/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package benchmark

import meerkat._
import java7.Java7_Regex._
import java.lang.management._
import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.mutable.ListBuffer
import com.google.common.testing.GcFinalization


object BenchmarkJavaRegex {
  
    val warmupCount: Int = 10
    val runCount: Int = 10
    val inputDir: String = "/Users/aliafroozeh/corpus/Java/jdk1.7.0_60-b19"
  
    val files: ListBuffer[File] = getFileNames(inputDir)

    import util.Configuration._
      
	def main(args: Array[String]) {
      
        val s = start(CompilationUnit)
      
        for (_ <- 0 until warmupCount) {
          val input = scala.io.Source.fromFile("test-files/warmup.java").mkString
          parse(input, s, ALL_PARSES)
        }
        
       GcFinalization.awaitFullGc()
      
        printf("%-20s %-20s %-20s %-20s %-20s %-15s %-15s\n", "size", "user_time", "nonterminal_nodes", "intermediate_nodes", "terminal_nodes", "packed_nodes", "ambiguous_nodes")
        
        for (f <- files)  {
			val input: String = scala.io.Source.fromFile(f).mkString
			
			println("#" + f)
			
			for(i <- 0 until runCount) {
			  val result = parse(input, s, ALL_PARSES)
			  
			  result match {
			    case ParseSuccess(nanoTime, userTime, cpuTime, sppf, countNonterminalNodes, countIntermediateNodes, countTerminalNodes, countPackedNodes, countAmbiguousNodes) => {
			    	printf("%-20d %-20d %-20d %-20d %-20d %-15d %-15d\n", input.length, userTime, countNonterminalNodes, countIntermediateNodes, countTerminalNodes, countPackedNodes, countAmbiguousNodes)
			    }
			    case ParseError(index, slot) => println("parse error")
			  }
			  
			  GcFinalization.awaitFullGc()
			}
        }      
	}
	
	def getFileNames(dir: String): ListBuffer[File] = {
	  val files = FileUtils.listFiles(new File(dir), Array("java"), true)
	  val it = files.iterator
	  val inputPaths: ListBuffer[File] = new ListBuffer
	  
	  while(it.hasNext()) {
	    inputPaths += (it.next().asInstanceOf[File])
	  }
	  
	  inputPaths
	}
}