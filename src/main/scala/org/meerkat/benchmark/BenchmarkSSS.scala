/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package benchmark

import meerkat._
import java.lang.management._
import com.google.common.testing._
import Gamma2.parse
import meerkat.Rule.memoize

object Gamma2 extends MeerkatParsers {
  val S: MeerkatParser = "S" ::=  S ~~ S ~~ S | S ~~ S | 'b'
}

object BenchmarkSSS {
  
    val warmupCount: Int = 5
    val runCount: Int = 10
    
    import Gamma2._
    import util.Configuration._
    
	def main(args: Array[String]) {
      
       for (i <- 0 to warmupCount) {
    	   val result = parse(getInput(400), S, ALL_PARSES)
       }
       
       GcFinalization.awaitFullGc()
       
       printf("%-20s %-20s %-20s %-20s %-20s %-15s %-15s\n", "size", "user_time", "nonterminal_nodes", "intermediate_nodes", "terminal_nodes", "packed_nodes", "ambiguous_nodes")
       
       for (i <- 10 to 500 by 10) {
    	   val input = getInput(i)
    	   
			for(i <- 0 until runCount) {
			  var result = parse(input, S, ALL_PARSES)
			  
			  result match {
			    case ParseSuccess(nanoTime, userTime, cpuTime, sppf, countNonterminalNodes, countIntermediateNodes, countTerminalNodes, countPackedNodes, countAmbiguousNodes) => {
			    	printf("%-20d %-20d %-20d %-20d %-20d %-15d %-15d\n", input.length, userTime, countNonterminalNodes, countIntermediateNodes, countTerminalNodes, countPackedNodes, countAmbiguousNodes)
			    }
			    case ParseError(index, slot) => println("parse error")
			  }

			  result = null
			  GcFinalization.awaitFullGc()
			}
        }      
	}
	
	def getInput(size: Int): String = {
	  var s = ""
	  for (_ <- 0 until size) s += "b"
	  s
	}
    
}