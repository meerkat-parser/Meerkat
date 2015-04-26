/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package sppf

import scala.collection.mutable._
import java.util.HashMap
import scala.collection.JavaConversions._

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

class DefaultSPPFLookup extends SPPFLookup {
  
  val terminalNodes: Map[TerminalNode, TerminalNode] = new HashMap[TerminalNode, TerminalNode]()
  val nonterminalNodes: Map[NonPackedNode, NonPackedNode] = new HashMap[NonPackedNode, NonPackedNode]()
  val intermediateNodes: Map[NonPackedNode, NonPackedNode] = new HashMap[NonPackedNode, NonPackedNode]()
  
  var countNonterminalNodes: Int = 0
  var countIntermediateNodes: Int = 0
  var countPackedNodes: Int = 0
  var countTerminalNodes: Int = 0
  var countAmbiguousNodes: Int = 0
  
  override def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode] =
    nonterminalNodes.get(NonterminalNode(name, leftExtent, rightExtent))

  override def getTerminalNode(s: Any, inputIndex: Int): TerminalNode = findOrElseCreateTerminalNode(s, inputIndex, inputIndex + 1)
  
  override def getTerminalNode(s: Any, leftExtent: Int, rightExtent: Int): TerminalNode = findOrElseCreateTerminalNode(s, leftExtent, rightExtent)
  
  override def getEpsilonNode(inputIndex: Int): TerminalNode = findOrElseCreateTerminalNode("epsilon", inputIndex, inputIndex)
    
  override def getNonterminalNode(head: Any, slot: Any, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {
    
    val leftExtent = if (leftChild.isDefined) leftChild.get.leftExtent else rightChild.leftExtent
    val rightExtent = rightChild.rightExtent
    val node = findOrElseCreateNonterminalNode(head, leftExtent, rightExtent)
    
    val packedNode = PackedNode(slot, rightChild.leftExtent, node)
    
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
    
    
    val packedNode = PackedNode(slot, rightChild.leftExtent, node)
    
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
    val key = TerminalNode(s, leftExtent, rightExtent)
    return terminalNodes.getOrElseUpdate(key, {countTerminalNodes += 1; key.init})
  }
  
  def findOrElseCreateNonterminalNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = NonterminalNode(slot, leftExtent, rightExtent)
    return nonterminalNodes.getOrElseUpdate(key, {countNonterminalNodes += 1; key.init})
  }
  
  def findOrElseCreateIntermediateNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = IntermediateNode(slot, leftExtent, rightExtent)
    return intermediateNodes.getOrElseUpdate(key, {countIntermediateNodes +=1; key.init});
  }
  
}

class OriginalSPPFLookup extends DefaultSPPFLookup {
  
  override def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode] =
    nonterminalNodes.get(OriginalNonterminalNode(name, leftExtent, rightExtent))
    
  override def findOrElseCreateNonterminalNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = OriginalNonterminalNode(slot, leftExtent, rightExtent)
    return nonterminalNodes.getOrElseUpdate(key, {countNonterminalNodes += 1; key.init})
  }
  
  override def findOrElseCreateIntermediateNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = OriginalIntermediateNode(slot, leftExtent, rightExtent)
    return intermediateNodes.getOrElseUpdate(key, {countIntermediateNodes +=1; key.init});
  }
  
}

