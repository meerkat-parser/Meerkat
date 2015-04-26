/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package gll

import java.util.ArrayDeque
import java.util.Deque
import sppf.NonPackedNode
import sppf.SPPFLookup
import sppf.TerminalNode
import util.DEBUG
import util.JavaUtilLoggerWrapper
import util.LoggerWrapper
import util.SEVERE
import sppf.OriginalSPPFLookup
import util.PrimeMultiplicatonHash

case class Descriptor(slot: Int, gssNode: GSSNode, i: Int, sppfNode: NonPackedNode) {
  
  	override def hashCode = PrimeMultiplicatonHash.hashCode(slot, i)
  
  	override def equals(o: Any) = o match {
  	  case other: Descriptor => slot == other.slot && i == other.i
  	  case _ => false
  	}
}

abstract class GLLParser(slotsSize: Int, inputSize: Int) {
  
  val log: LoggerWrapper = new JavaUtilLoggerWrapper()
  log.setLevel(SEVERE)
  
  val L0 = -1
  val u0 = GSSNode(-1, 0)
  val $ = TerminalNode(-1, -1, -1)
  
  var slot: Int = L0
  var cu: GSSNode = u0 
  var ci: Int = 0
  var cn: NonPackedNode = $

  val gssLookup: GSSLookup = new ArrayGSSLookup(slotsSize, inputSize)
  val sppfLookup: SPPFLookup = new OriginalSPPFLookup
  val descriptors: Deque[Descriptor] = new ArrayDeque[Descriptor]
  
  var descriptorsCount: Int = 0

  def isEndSlot(slot: Int): Boolean
  
  def isFirstSlot(slot: Int): Boolean
  
  def create(slot: Int, u: GSSNode, i: Int, node: NonPackedNode): Boolean = {
    
    gssLookup.findGSSNode(slot, i) match {
      
      case None => {
        val v = gssLookup.createGSSNode(slot, i)
    	val gssEdge = GSSEdge(node, u)
    	v.outgoing += gssEdge
    	cu = v
    	return true
      }
      
      case Some(v) => {
        val gssEdge = GSSEdge(node, u)
    	v.outgoing += gssEdge
        for (z: NonPackedNode <- v.poppedElements) {
          val y = getNodeP(-1, slot, node, z)
          add(Descriptor(slot, u, y.rightExtent, y))
        }
        return false
      }
    }
  }
  
  def add(d: Descriptor) {
    if (!d.gssNode.hasDescriptor(d)) {
      schedule(d)
    }
  }
  
  /**
   * Does not add the descriptor to the duplicate set, only
   * schedules them for running
   */
  def schedule(d: Descriptor) {
     descriptors.add(d)
     descriptorsCount += 1
     log.debug("Descriptor added %s", d)
  }
  
  def pop(head: Int, i: Int, u: GSSNode, z: NonPackedNode) {
    if (u != u0 && !u.hasPoppedElement(i)) {
	    u.addToPoppedElements(z)    
	    for (e : GSSEdge <- u.outgoing) {
	      val y = getNodeP(head, u.slot , e.node, z)
	      add(Descriptor(u.slot, e.target, i, y))
	    }
    }
 }
  
 def getNodeP(head: Int, slot: Int, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode  = {
   if (isFirstSlot(slot)) return rightChild
   
   if (isEndSlot(slot)) {
      leftChild match {
        case $ => sppfLookup.getNonterminalNode(head, slot, rightChild)
        case _ => sppfLookup.getNonterminalNode(head, slot, Some(leftChild), rightChild)
      }            
   } else {
	  leftChild match {
	    case $ => sppfLookup.getIntermediateNode(slot, None, rightChild)
	    case _ => sppfLookup.getIntermediateNode(slot, leftChild, rightChild)
	  }
   }
 }
  
}