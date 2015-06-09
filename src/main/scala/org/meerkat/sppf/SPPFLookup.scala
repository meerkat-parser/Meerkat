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
import java.util.HashMap
import scala.collection.JavaConversions._
import org.meerkat.util.IntKey3
import org.meerkat.util.Input
import org.meerkat.util.IntKey3

trait SPPFLookup {
  def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode]
  def getTerminalNode(s: Any, inputIndex: Int): TerminalNode
  def getTerminalNode(s: Any, leftExtent: Int, rightExtent: Int): TerminalNode
  def getEpsilonNode(inputIndex: Int): TerminalNode
  def getNonterminalNode(head: Any, slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode
  def getNonterminalNode(head: Any, slot: Slot, rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Slot, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode
  def countNonterminalNodes: Int
  def countIntermediateNodes: Int
  def countPackedNodes: Int
  def countTerminalNodes: Int
  def countAmbiguousNodes: Int
}

class DefaultSPPFLookup(input: Input) extends SPPFLookup {
  
  val n = input.length
  val hash = (k1: Int, k2: Int, k3: Int) => k1 * n * n + k2 * n + k3
  
  val terminalNodes:     Map[IntKey3, TerminalNode]  = new HashMap[IntKey3, TerminalNode]()
  val nonterminalNodes:  Map[IntKey3, NonPackedNode] = new HashMap[IntKey3, NonPackedNode]()
  val intermediateNodes: Map[IntKey3, NonPackedNode] = new HashMap[IntKey3, NonPackedNode]()
  
  var countNonterminalNodes: Int = 0
  var countIntermediateNodes: Int = 0
  var countPackedNodes: Int = 0
  var countTerminalNodes: Int = 0
  var countAmbiguousNodes: Int = 0
  
  override def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode] =
    nonterminalNodes.get(IntKey3(name.hashCode(), leftExtent, rightExtent, hash))

  override def getTerminalNode(s: Any, inputIndex: Int): TerminalNode = findOrElseCreateTerminalNode(s, inputIndex, inputIndex + 1)
  
  override def getTerminalNode(s: Any, leftExtent: Int, rightExtent: Int): TerminalNode = findOrElseCreateTerminalNode(s, leftExtent, rightExtent)
  
  override def getEpsilonNode(inputIndex: Int): TerminalNode = findOrElseCreateTerminalNode("epsilon", inputIndex, inputIndex)
    
  override def getNonterminalNode(head: Any, slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {
    
    val leftExtent = if (leftChild.isDefined) leftChild.get.leftExtent else rightChild.leftExtent
    val rightExtent = rightChild.rightExtent
    val node = findOrElseCreateNonterminalNode(head, leftExtent, rightExtent)
    
    val packedNode = PackedNode(slot, node)
    
    val ambiguousBefore = node.isAmbiguous
    if (node.addPackedNode(packedNode, leftChild, rightChild)) countPackedNodes += 1
    val ambiguousAfter = node.isAmbiguous
    
    if (!ambiguousBefore && ambiguousAfter) {
      println(head.toString() + "(" + leftExtent + ", " + rightExtent + ")")
      countAmbiguousNodes += 1 
    }
    
    return node
  }
  
  override def getNonterminalNode(head: Any, slot: Slot, rightChild: NonPackedNode): NonPackedNode = {
    return getNonterminalNode(head, slot, None, rightChild)
  }
  
  override def getIntermediateNode(slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {
    
    val leftExtent = if (leftChild.isDefined) leftChild.get.leftExtent else rightChild.leftExtent 
    val rightExtent = rightChild.rightExtent
    val node =  findOrElseCreateIntermediateNode(slot, leftExtent, rightExtent)
    
    val packedNode = PackedNode(slot, node)
    
    val ambiguousBefore = node.isAmbiguous
    if (node.addPackedNode(packedNode, leftChild, rightChild)) countPackedNodes += 1
    val ambiguousAfter = node.isAmbiguous
    
    if (!ambiguousBefore && ambiguousAfter) countAmbiguousNodes += 1 
    
    return node
  }
  
  override def getIntermediateNode(slot: Slot, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode = {
    return getIntermediateNode(slot, Some(leftChild), rightChild);
  }
  
  def findOrElseCreateTerminalNode(s: Any, leftExtent: Int, rightExtent: Int): TerminalNode = {
    val key = IntKey3(s.hashCode(), leftExtent, rightExtent, hash)
    return terminalNodes.getOrElseUpdate(key, {countTerminalNodes += 1; TerminalNode(s, leftExtent, rightExtent)})
  }
  
  def findOrElseCreateNonterminalNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = IntKey3(slot.hashCode(), leftExtent, rightExtent, hash) 
    return nonterminalNodes.getOrElseUpdate(key, {countNonterminalNodes += 1; NonterminalNode(slot, leftExtent, rightExtent)})
  }
  
  def findOrElseCreateIntermediateNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = IntKey3(slot.hashCode(), leftExtent, rightExtent, hash)
    return intermediateNodes.getOrElseUpdate(key, {countIntermediateNodes +=1; IntermediateNode(slot, leftExtent, rightExtent)});
  }
  
}
