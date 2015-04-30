/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package org.meerkat.sppf 

import scala.collection.mutable._
import org.meerkat.util.PrimeMultiplicatonHash
import scala.collection.JavaConversions._
import java.util.ArrayList

trait SPPFNode {
  def children: Buffer[SPPFNode]
}

trait NonPackedNode extends SPPFNode {
  
  var children: Buffer[SPPFNode] = null
  
	val name: Any
	val leftExtent, rightExtent: Int
	
	def init: NonPackedNode 
	
	def addPackedNode(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): Boolean = {
      attachChildren(packedNode, leftChild, rightChild)      
      children += packedNode
      return true
    }
		
	def attachChildren(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode) = {
	  if (leftChild.isDefined) packedNode.leftChild = leftChild.get
      packedNode.rightChild = rightChild
	}
	
	def isAmbiguous: Boolean = children.size > 1
	
	override def toString  = name + "," + leftExtent + "," + rightExtent
	
	override def hashCode: Int = PrimeMultiplicatonHash.hashCode(name.hashCode, leftExtent, rightExtent)
}

trait OriginalNonPackedNode extends NonPackedNode {
   
   var packedNodeSet: Set[PackedNode] = null
   
   override def addPackedNode(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): Boolean = {
      if (packedNodeSet.contains(packedNode)) {
        return false
      }
      packedNodeSet.add(packedNode)
      attachChildren(packedNode, leftChild, rightChild)      
      children += packedNode
      return true
    }   
}

case class NonterminalNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
  def init: NonterminalNode = {children = new ArrayList[SPPFNode](); this}
}

case class IntermediateNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
  def init: IntermediateNode = {children = new ArrayList[SPPFNode](); this}
}

case class TerminalNode(s: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
	
    def init: TerminalNode = {this}
  
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
	
	override val name = s
}

case class PackedNode(name: Any, parent: NonPackedNode) extends SPPFNode {

    var leftChild: NonPackedNode = null
    var rightChild: NonPackedNode = null
    
    def pivot = leftChild.rightExtent
    
    var values: List[SPPFNode] = null
    
    def children: Buffer[SPPFNode] = {
      val l: Buffer[SPPFNode] = new ArrayList[SPPFNode]()
      if (leftChild != null) l += leftChild
      if (rightChild != null) l += rightChild
      return l
    }
  
	override def toString = name + "," + pivot + ", parent=(" + parent + ")"
}
