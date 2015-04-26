/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package gll

import sppf.TerminalNode
import sppf.NonPackedNode
import util.Input
import util.LoggerWrapper
import util.JavaUtilLoggerWrapper
import util.Visualization
import util._


/**
 * 
 * S ::= S S S 
 *     | S S
 *     | b
 * 
 */
object Gamma  {
  
  def main(args: Array[String]) {
	
	printf("%-20s %-20s %-20s %-20s %-20s %-20s %-20s %-15s %-15s %-15s\n", "size", "user_time", "cpu_time", "nano_time", "descriptors", "nonterminal_nodes", "intermediate_nodes", "nonpacked_nodes", "terminal_nodes", "packed_nodes", "ambiguous_nodes")

    for (i <- 1 to 4) {
    	val input = new Input(getString(400))
    	val gamma: Gamma = new Gamma(10, input.length)
		gamma.parse(input)      
    }
	
    for (i <- 10 to 500 by 10) {
      for (_ <- 1 to 10) {
        val input = new Input(getString(i))
    	val gamma: Gamma = new Gamma(10, input.length)
      
		val startUserTime = getUserTime
		val startSystemTime = getSystemTime
		val startNanoTime = System.nanoTime
	    
		gamma.parse(input)
		
	    val endUserTime: Long = getUserTime
		val endSystemTime = getSystemTime
		val endNanoTime: Long = System.nanoTime
		
	    printf("%-20d %-20d %-20d %-20d %-20d %-20d %-20d %-20d %-15d %-15d %-15d\n", 
	    	   input.length, 
	    	   (endUserTime - startUserTime) / 1000000,
	    	   (endSystemTime - startSystemTime) / 1000000, 
	    	   (endNanoTime - startNanoTime) / 1000000,
	    	   gamma.descriptorsCount,
	    	   gamma.sppfLookup.countNonterminalNodes,
	    	   gamma.sppfLookup.countIntermediateNodes,
	    	   gamma.sppfLookup.countNonterminalNodes + gamma.sppfLookup.countIntermediateNodes,
	    	   gamma.sppfLookup.countTerminalNodes,
	    	   gamma.sppfLookup.countPackedNodes, 
	    	   gamma.sppfLookup.countAmbiguousNodes)
    	} 
      }    
  }
 
  def getString(size: Int): String = {
    var s = ""
	for (_ <- 0 until size) s += "b"
	s
  } 
}
  
  
class Gamma(slotsSize: Int, inputSize: Int) extends GLLParser(slotsSize: Int, inputSize: Int) {
  def parse(input: Input): Option[NonPackedNode] = {
    
    // Adding descriptors for S
    schedule(Descriptor(2, cu, ci, cn))
    schedule(Descriptor(6, cu, ci, cn))
    schedule(Descriptor(9, cu, ci, cn))
    
    while (!descriptors.isEmpty()) {
         
        slot match {
      
          case L0 => {
            val d = descriptors.pop()
            cu = d.gssNode 
            ci = d.i
            cn = d.sppfNode
            slot = d.slot
	      }
      
	      // S
	      case 1 => {
	        add(Descriptor(2, cu, ci, cn))
	        add(Descriptor(6, cu, ci, cn))
	        add(Descriptor(9, cu, ci, cn))
	        slot = L0
	      }
	      
	      // S ::= . S S S
	      case 2 => {
	        if (create(3, cu, ci, cn)) // S ::= S . S S
	        	slot = 1
	        else
	           slot = L0
	      }
	      
	      // S ::= S . S S
	      case 3 => {
	        if (create(4, cu, ci, cn)) // S ::= S S . S
	        	slot = 1
	        else 
	        	slot = L0 
	      }
	      
	      // S ::= S S . S
	      case 4 => {
	        if (create(5, cu, ci, cn)) // S ::= S S S .
	        	slot = 1
	        else
	            slot = L0
	      }
	      
	      // S ::= S S S .
	      case 5 => {
	        pop(1, ci, cu, cn)
	        slot = L0
	      }
	
	      // S ::= . S S
	      case 6 => {
	        if (create(7, cu, ci, cn)) // S ::= S . S
	        	slot = 1
	        else 
	        	slot = L0  
	      }
	      
	      // S ::= S . S
	      case 7 => {
	        if (create(8, cu, ci, cn)) // S ::= S S .
	        	slot = 1
	        else
	            slot = L0
	      }
	      
	      // S ::= S S .
	      case 8 => {
	        pop(1, ci, cu, cn)
	        slot = L0        
	      }
	      
	      // S ::= . b
	      case 9 => {
	        if (ci < input.length && input.charAt(ci) == 'b') {
	          val cr = sppfLookup.getTerminalNode('b', ci)
	          cn = sppfLookup.getNonterminalNode(1, 10, None, cr)
	          ci += 1;
		      pop(1, ci, cu, cn)          
	        }
	        slot = L0
	      }
      }
    }

    sppfLookup.getStartNode(1, 0, input.length)    
  }  
  
  override def isEndSlot(slot: Int): Boolean = {
    
    slot match {

      // S ::= S S S .
      case 5 => true
      
      // S ::= S S .
      case 8 => true
      
      // S ::= b .
      case 10 => true
      
      case _ => false      
    }
  }
  
  override def isFirstSlot(slot: Int): Boolean = {
    
    slot match {

      // S ::= S . S S
      case 3 => true
      
      // S ::= S . S
      case 7 => true
      
      case _ => false
    }
  }
}
	    
