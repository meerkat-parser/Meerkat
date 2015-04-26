/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

import sppf.SPPFNode
import scala.collection.mutable._
import sppf.NonterminalNode
import sppf.IntermediateNode
import sppf.TerminalNode
import sppf.PackedNode

object Ambiguity {

    def countAmbiguities(node: SPPFNode): Int =  {
       val ambiguousNodes: Set[SPPFNode] = new HashSet
       countAmbiguities(node, ambiguousNodes, new HashSet)
//       for(n <- ambiguousNodes) {
//         Visualization.toDot(n)
//         return ambiguousNodes.size
//       }
       ambiguousNodes.size
    } 
      
  
  	def countAmbiguities(node: SPPFNode, ambiguousNodes: Set[SPPFNode], duplicateSet: Set[SPPFNode]) : Unit = {
	  
	    if (!duplicateSet.contains(node)) {
	      duplicateSet.add(node)
	      
      	  node match {
      	     case n@NonterminalNode(slot, leftExtent, rightExtent) => 
      	       if (n.isAmbiguous) ambiguousNodes.add(n)
      	       for(t <- n.children) countAmbiguities(t, ambiguousNodes, duplicateSet)
		    
		     case n@IntermediateNode(slot, leftExtent, rightExtent) =>
		       if (n.isAmbiguous) ambiguousNodes.add(n)
		       for(t <- n.children) countAmbiguities(t, ambiguousNodes, duplicateSet)
		       
			 case n@TerminalNode(char, leftExtent, rightExtent) =>
			    
			 case n@PackedNode(slot, pivot, parent) =>
			   if(n.leftChild != null) countAmbiguities(n.leftChild, ambiguousNodes, duplicateSet)
			   countAmbiguities(n.rightChild, ambiguousNodes, duplicateSet)
	      }
	    } 
	}
  
}
