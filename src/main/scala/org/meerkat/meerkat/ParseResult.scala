/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import sppf.SPPFNode

trait ParseResult
  
case class ParseSuccess(nanoTime: Long, 
		  				userTime: Long,
		  				systemTime: Long,
		  				sppf: SPPFNode,
		  				countNonterminaldNodes: Int,
		  				countIntermediateNodes: Int,
		  				countTerminalNodes: Int,
		  				countPackedNodes: Int,
		  				countAmbiguousNodes: Int) extends ParseResult
		  				  
case class ParseError(index: Int, slot: String) extends ParseResult