/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.sppf 

import scala.collection.mutable._
import org.meerkat.util.PrimeMultiplicatonHash
import scala.collection.JavaConversions._
import java.util.ArrayList
import org.meerkat.tree._

trait Slot {
  def ruleType: Rule
}

trait SPPFNode {
	type T <: SPPFNode
  def children: Seq[T]
  def size: Int
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
  
  def size = {
   var s = 0
   if (first != null) s += 1
   if (rest != null) s += rest.size
   s
  }
  
  def isIntermediateNode: Boolean = this.isInstanceOf[IntermediateNode]
  	
	override def toString  = name + "," + leftExtent + "," + rightExtent
}

case class NonterminalNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode

case class IntermediateNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode 

trait AbstractTerminalNode

case class TerminalNode(s: String, leftExtent: Int, rightExtent: Int) extends NonPackedNode with AbstractTerminalNode {
	
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
	
	override val name: String = s
}

case class LayoutTerminalNode(s: String, leftExtent: Int, rightExtent: Int) extends NonPackedNode with AbstractTerminalNode {
  override val name: String = s
}

case class PackedNode(slot: Slot, parent: NonPackedNode) extends SPPFNode {

  type T = NonPackedNode
  
  var leftChild: T = _
  var rightChild: T = _
    
  def pivot = leftChild.rightExtent
    
  def ruleType: Rule = slot.ruleType
    
  def children: Buffer[T] = ListBuffer(leftChild, rightChild) filter (_ != null)
  
  def size = if (hasRightChild) 2 else 1 
  
  def hasRightChild: Boolean  = rightChild != null
  
	override def toString = slot + "," + pivot + ", parent=(" + parent + ")"

  override def equals(o: Any) = o match {
    case p: PackedNode => slot == p.slot && parent == p.parent && pivot == p.pivot
    case _             => false
  }
}

