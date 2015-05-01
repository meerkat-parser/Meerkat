/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
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
  def getNonterminalNode(head: Any, slot: Any, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode
  def getNonterminalNode(head: Any, slot: Any, rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Any, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode
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
    
  override def getNonterminalNode(head: Any, slot: Any, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {
    
    val leftExtent = if (leftChild.isDefined) leftChild.get.leftExtent else rightChild.leftExtent
    val rightExtent = rightChild.rightExtent
    val node = findOrElseCreateNonterminalNode(head, leftExtent, rightExtent)
    
    val packedNode = PackedNode(slot, node)
    
    val ambiguousBefore = node.isAmbiguous
    if (node.addPackedNode(packedNode, leftChild, rightChild)) countPackedNodes += 1
    val ambiguousAfter = node.isAmbiguous
    
    if (!ambiguousBefore && ambiguousAfter) countAmbiguousNodes += 1 
    
    return node
  }
  
  override def getNonterminalNode(head: Any, slot: Any, rightChild: NonPackedNode): NonPackedNode = {
    return getNonterminalNode(head, slot, None, rightChild)
  }
  
  override def getIntermediateNode(slot: Any, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {
    
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
  
  override def getIntermediateNode(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode = {
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
