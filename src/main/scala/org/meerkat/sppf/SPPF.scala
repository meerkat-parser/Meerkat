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
import org.meerkat.tree.Rule
import org.meerkat.tree.Rule
import org.meerkat.tree.Nonterminal
import org.meerkat.tree.Terminal
import org.meerkat.tree.AbstractRule

trait Slot {
  def ruleType: AbstractRule
}

trait SPPFNode {
	type T <: SPPFNode
  def children: Seq[T]
  def flatChildren: ListBuffer[SPPFNode]
}

trait NonPackedNode extends SPPFNode {
 
  type T = PackedNode
  
  var first: T = _
  
  var rest: Buffer[T] = _
  
	val name: Any
	val leftExtent, rightExtent: Int
  
  def children: Seq[T] = {
    if (first == null) ListBuffer()
    else if (rest == null) ListBuffer(first)
    else ListBuffer(first) ++ rest
  }
	
	def init: Unit = rest = new ArrayList[T]() 
	
	def addPackedNode(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): Boolean = {
      attachChildren(packedNode, leftChild, rightChild)
      if (first == null) {
        first = packedNode
      } else { 
        if (rest == null) init
        rest += packedNode
      }
      
      return true
    }
		
	def attachChildren(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode) = leftChild match {
    case Some(c) => packedNode.leftChild = c; packedNode.rightChild = rightChild
    case None    => packedNode.leftChild = rightChild
   }
	
	def isAmbiguous: Boolean = rest != null
  
  def hasChildren: Boolean = first != null || rest != null
  
  def isIntermediateNode: Boolean = this.isInstanceOf[IntermediateNode]
  
  override def flatChildren: ListBuffer[SPPFNode] = flatChildren(this)
  
  private def flatChildren(node: NonPackedNode): ListBuffer[SPPFNode] = {
   if (isAmbiguous) {
     ListBuffer(node)
   } else {
     val l = ListBuffer[SPPFNode]()
     if (node.hasChildren && node.first.leftChild.isIntermediateNode) 
       l ++= flatChildren(node.first.leftChild) 
     else 
       l += node.first.leftChild
       
     if (node.first hasRightChild) l += node.first.rightChild
     l
   } 
  }
	
	override def toString  = name + "," + leftExtent + "," + rightExtent
}

case class NonterminalNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode

case class IntermediateNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode 

case class TerminalNode(s: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
	
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
	
	override val name: String = s toString
}

case class PackedNode(slot: Slot, parent: NonPackedNode) extends SPPFNode {

  type T = NonPackedNode
  
  var leftChild: T = _
  var rightChild: T = _
    
  def pivot = leftChild.rightExtent
    
  def rule: AbstractRule = slot.ruleType
    
  def children: Buffer[T] = ListBuffer(leftChild, rightChild) filter (_ != null)
  
  def hasRightChild: Boolean  = rightChild != null
  
  override def flatChildren = {
    val l = ListBuffer[SPPFNode]()
    if (leftChild isIntermediateNode)
      l ++= leftChild.flatChildren
    else 
      l += leftChild

    if (hasRightChild)
      if (rightChild isIntermediateNode)
        l ++= rightChild.flatChildren
      else 
        l += rightChild
    l      
  }
  
	override def toString = slot + "," + pivot + ", parent=(" + parent + ")"

  override def equals(o: Any) = o match {
    case p: PackedNode => slot == p.slot && parent == p.parent && pivot == p.pivot
    case _             => false
  }
}

