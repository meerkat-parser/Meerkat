package org.meerkat.util

import org.meerkat.sppf.SPPFNode
import scala.collection.mutable._
import org.meerkat.sppf.NonterminalNode
import org.meerkat.sppf.IntermediateNode
import org.meerkat.sppf.TerminalNode
import org.meerkat.sppf.PackedNode

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
			    
			 case n@PackedNode(slot, parent) =>
			   if(n.leftChild != null) countAmbiguities(n.leftChild, ambiguousNodes, duplicateSet)
			   countAmbiguities(n.rightChild, ambiguousNodes, duplicateSet)
	      }
	    } 
	}
  
}
