/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package org.meerkat.parsers

import org.meerkat.sppf.NonPackedNode

case class ParseSuccess(sppf: NonPackedNode, stat: ParseStatistics)
		  				  
case class ParseError(index: Int, slot: String) {
  override def toString = s"Parse error at $slot and $index"
}

case class ParseStatistics(nanoTime: Long, 
                           userTime: Long,
                           systemTime: Long,
                           countNonterminalNodes: Int,
                           countIntermediateNodes: Int,
                           countTerminalNodes: Int,
                           countPackedNodes: Int,
                           countAmbiguousNodes: Int) {

  override def toString = "%-20d %-20d %-20d %-20d %-15d %-15d\n".format(userTime, countNonterminalNodes, countIntermediateNodes, countTerminalNodes, countPackedNodes, countAmbiguousNodes)
}