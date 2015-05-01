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
	type T <: SPPFNode
  def children: Buffer[T]
  def flatChildren: List[SPPFNode]
}

trait NonPackedNode extends SPPFNode {
 
  type T = PackedNode
  
  var first:T = null
  
  var children: Buffer[T] = null
  
	val name: Any
	val leftExtent, rightExtent: Int
	
	def init: Unit = children = new ArrayList[T]() 
	
	def addPackedNode(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): Boolean = {
      attachChildren(packedNode, leftChild, rightChild)
      if (first == null)
        first = packedNode
      else { init; children += packedNode }
      
      return true
    }
		
	def attachChildren(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode) = {
	  if (leftChild.isDefined) packedNode.leftChild = leftChild.get
    packedNode.rightChild = rightChild
	}
	
	def isAmbiguous: Boolean = children != null
	
	override def toString  = name + "," + leftExtent + "," + rightExtent
}

case class NonterminalNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
  override def flatChildren = List(this)
}

case class IntermediateNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
  override def flatChildren: List[SPPFNode] = {
    if (first == null) 
      List()
    else 
      first.flatChildren ++ children flatMap { c => flatChildren }
  } 
}

case class TerminalNode(s: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
	
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
	
	override val name = s
  
  override def flatChildren = List()
}

case class PackedNode(name: Any, parent: NonPackedNode) extends SPPFNode {

    type T = NonPackedNode
  
    var leftChild: T = null
    var rightChild: T = null
    
    def pivot = leftChild.rightExtent
    
    var values: List[SPPFNode] = null
    
    def children: Buffer[T] = {
      val l: Buffer[T] = new ArrayList[T]()
      if (leftChild != null) l += leftChild
      if (rightChild != null) l += rightChild
      return l
    }
  
	override def toString = name + "," + pivot + ", parent=(" + parent + ")"
  
  override def flatChildren = List()
}
