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

trait SPPFNode {
	type T <: SPPFNode
  def children: Seq[T]
  def flatChildren: Iterable[SPPFNode] = ???
}

class Blah extends Iterable[SPPFNode] {
  override def iterator: Iterator[SPPFNode] = new Iterator[SPPFNode] {
    def hasNext: Boolean = ???
    def next: SPPFNode = ???
  }
}

trait NonPackedNode extends SPPFNode {
 
  type T = PackedNode
  
  var first: T = null
  
  var rest: Buffer[T] = null
  
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
		
	def attachChildren(packedNode: PackedNode, leftChild: Option[NonPackedNode], rightChild: NonPackedNode) = {
	  if (leftChild.isDefined) packedNode.leftChild = leftChild.get
    packedNode.rightChild = rightChild
	}
	
	def isAmbiguous: Boolean = children != null
	
	override def toString  = name + "," + leftExtent + "," + rightExtent
}

case class NonterminalNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
//  override def flatChildren: Iterable[SPPFNode] = new Iterable[SPPFNode] {
//    override def iterator: Iterator[SPPFNode] = new Iterator[SPPFNode] {
//      
//      var nextNode = this
//      
//      while (nextNode != null) {
//        nextNode match {
//          case n: NonterminalNode => nextNode = n   
//        }
//      }
//      
//      def hasNext: Boolean = ???
//      def next: SPPFNode = ???
//    }
//  }
}

case class IntermediateNode(name: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode 

case class TerminalNode(s: Any, leftExtent: Int, rightExtent: Int) extends NonPackedNode {
	
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
	
	override val name: String = s toString
}

case class PackedNode(name: Any, parent: NonPackedNode) extends SPPFNode {

  type T = NonPackedNode
  
  var leftChild: T = _
  var rightChild: T = _
    
  def pivot = rightChild.leftExtent
    
  def rule: Rule = ???
    
  def children: Buffer[T] = ListBuffer(leftChild, rightChild) filter (_ != null)
  
	override def toString = name + "," + pivot + ", parent=(" + parent + ")"

  override def equals(o: Any) = o match {
    case p: PackedNode => name == p.name && parent == p.parent && pivot == p.pivot
    case _             => false
  }
}

