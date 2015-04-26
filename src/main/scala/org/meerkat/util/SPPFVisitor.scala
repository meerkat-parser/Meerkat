/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

import sppf._
import scala.collection.mutable._

object SPPFVisitor {
  
  	def removePackedNodes(node: SPPFNode, duplicateSet: Set[SPPFNode]): Unit = {
  	    if (!duplicateSet.contains(node)) {
	      duplicateSet.add(node)
	    } else {
	      return
	    }

	  node match {
	      case n@NonterminalNode(slot, leftExtent, rightExtent) =>
	        for(child <- n.children) removePackedNodes(child, duplicateSet)
	        
	        if (n.children.size == 1) {
	          val childrenOfChildren = n.children(0).children
		      n.children.clear
		      n.children ++= childrenOfChildren
	        }
	    
	      case n@IntermediateNode(slot, leftExtent, rightExtent) =>
	        for(child <- n.children) removePackedNodes(child, duplicateSet)
   	        if (n.children.size == 1) {
	          val childrenOfChildren = n.children(0).children
		      n.children.clear
		      n.children ++= childrenOfChildren
	        }

	        
		  case n@TerminalNode(char, leftExtent, rightExtent) =>
		    for(child <- n.children) removePackedNodes(child, duplicateSet)
		    
		  case n@PackedNode(slot, pivot, parent) =>
		    // for(child <- n.children) removePackedNodes(child, duplicateSet)
	  }
	}
	
	def removeIntermediateNodes(node: SPPFNode, duplicateSet: Set[SPPFNode]): Unit = {

    	if (!duplicateSet.contains(node)) {
	      duplicateSet.add(node)
	    } else {
	      return
	    }
	  
      for(child <- node.children) 
        removeIntermediateNodes(child, duplicateSet)

	  node match {
       	case n@IntermediateNode(slot, leftExtent, rightExtent) =>
	        n.children(0) match {
	        	case firstIntermediate@IntermediateNode(slot, leftExtent, rightExtent) =>
			      val childrenOfFirst = firstIntermediate.children
			      n.children.remove(0)
			      n.children.++=:(childrenOfFirst)
	          	case _ =>
	        }

       	case n@NonterminalNode(slot, leftExtent, rightExtent) =>
	        n.children(0) match {
	        	case firstIntermediate@IntermediateNode(slot, leftExtent, rightExtent) =>
			      val childrenOfFirst = firstIntermediate.children
			      n.children.remove(0)
			      n.children.++=:(childrenOfFirst)
	          	case _ =>
	        }
	        
       	case n@PackedNode(slot, leftExtent, rightExtent) =>
	        n.children(0) match {
	        	case firstIntermediate@IntermediateNode(slot, leftExtent, rightExtent) =>
			      val childrenOfFirst = firstIntermediate.children
			      n.children.remove(0)
			      n.children.++=:(childrenOfFirst)
	          	case _ =>
	        }
	        
		  case _ =>
        	  
	  	}
	}
	
//	def find(label: String, t: SPPFNode): Option[NonPackedNode] = {
//	  t match {
//	    case n: NonterminalNode => 
//	      if(n.labeled(label)) 
//	        return Option(n)
//	      else
//	        for(n <- t.children) {
//	          val node = find(label, n)
//	          if(node.isDefined) return node
//	        }
//	    case n: TerminalNode => 
//	      if(n.labeled(label)) 
//	        return Option(n)
//	    case _ =>
//	      for(n <- t.children) {
//	        val node = find(label, n)
//	        if(node.isDefined) return node
//	      }
//	  }	  
//	  None
//	}

}