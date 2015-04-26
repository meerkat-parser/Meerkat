/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package gll

import scala.collection.mutable._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._
import sppf.SPPFNode
import java.util.HashMap
import java.util.ArrayList
import java.util.HashSet
import sppf.NonPackedNode

case class GSSNode(slot: Int, i: Int) {
  val outgoing: Buffer[GSSEdge] = new ArrayList[GSSEdge]()
  val poppedElements: Buffer[NonPackedNode] = new ArrayList[NonPackedNode]()
 
  private val set: Set[Int] = new HashSet[Int]()
  
  def addToPoppedElements(node: NonPackedNode) = {
	  if (!set.contains(node.rightExtent)) {
	    set.add(node.rightExtent)
	    poppedElements.add(node)
	  } 
  }
  
  def hasPoppedElement(i: Int) = set.contains(i)
  
  private val descriptorsSet:Set[Descriptor] = new HashSet[Descriptor]()
  
  def hasDescriptor(descriptor: Descriptor) = {
    if (!descriptorsSet.contains(descriptor)) {
      descriptorsSet.add(descriptor)
      false
    } else {
      true
    }
  }
  
}

case class GSSEdge(node: NonPackedNode, target: GSSNode)

trait GSSLookup {
  def createGSSNode(slot: Int, i: Int): GSSNode
  def findGSSNode(slot: Int, i: Int): Option[GSSNode]
}


class HashGSSLookup extends GSSLookup {
  
	val gssNodes: Map[GSSNode, GSSNode] = new HashMap[GSSNode, GSSNode]()
	
	override def createGSSNode(slot: Int, i: Int): GSSNode = {
	  val key: GSSNode = GSSNode(slot, i)
	  gssNodes.put(key, key)
	  key
	}
	
	override def findGSSNode(slot: Int, i: Int): Option[GSSNode] = gssNodes.get(GSSNode(slot, i))	
}

class ArrayGSSLookup(slotSize: Int, inputSize: Int) extends GSSLookup {
	var gssNodes = Array.ofDim[GSSNode](slotSize, inputSize + 1)
	
	override def createGSSNode(slot: Int, i: Int): GSSNode = {
	  val key: GSSNode = GSSNode(slot, i)
	  gssNodes(slot)(i) = key
	  key
	}
	
	override def findGSSNode(slot: Int, i: Int): Option[GSSNode] = 
	  gssNodes(slot)(i) match {
	  	case null => None
	  	case n => Some(n)
	  }
}

